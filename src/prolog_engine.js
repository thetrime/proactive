"use strict";

var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var Constants = require('./constants');
var foreign_module = require('./proactive_foreign.js');
var Prolog = require('proscript');

var getInitialStateFunctor = Prolog._make_functor(Prolog._make_atom("getInitialState"), 2);
var componentWillReceivePropsFunctor = Prolog._make_functor(Prolog._make_atom("componentWillReceiveProps"), 3);
var updateMessageHandlersFunctor = Prolog._make_functor(Prolog._make_atom("updateMessageHandlers"), 6);
var onMessageFunctor = Prolog._make_functor(Prolog._make_atom("onMessage"), 5);
var renderFunctor = Prolog._make_functor(Prolog._make_atom("render"), 3);
var documentFunctor = Prolog._make_functor(Prolog._make_atom("document"), 1);
var createElementFromVDomFunctor = Prolog._make_functor(Prolog._make_atom("create_element_from_vdom"), 3);
var vDiffFunctor = Prolog._make_functor(Prolog._make_atom("vdiff"), 3);
var vMutateFunctor = Prolog._make_functor(Prolog._make_atom("vmutate"), 5);
var vPatchFunctor = Prolog._make_functor(Prolog._make_atom("vpatch"), 4);
var expandChildrenFunctor = Prolog._make_functor(Prolog._make_atom("expand_children"), 2);
var messageFunctor = Prolog._make_functor(Prolog._make_atom("message"), 3);
var userFunctor = Prolog._make_functor(Prolog._make_atom("user"), 1);
var systemFunctor = Prolog._make_functor(Prolog._make_atom("system"), 1);
var consultedFunctor = Prolog._make_functor(Prolog._make_atom("consulted"), 1);
var pongAtom = Prolog._make_atom("pong");
var server_connection = null;
var qOp = null;
var pinger = null;
function crossModuleCall(module, goal)
{
    return Prolog._make_compound(Constants.crossModuleCallFunctor, [Prolog._make_atom(module), goal]);
}

function install_foreign()
{
    // load in the proactive foreign predicates
    var foreign_predicates = Object.keys(foreign_module);
    for (var p = 0; p < foreign_predicates.length; p++)
        Prolog.define_foreign(foreign_predicates[p], foreign_module[foreign_predicates[p]]);
}

function PrologEngine(baseURL, rootElementId, errorHandler, messageHandler, onConnectionLoss, callback)
{
    this.env = {};
    this.busy = 0;
    this.rendering = false;
    this.event_queue = [];
    this.processing_event = 0;
    this.errorHandler = errorHandler;
    this.messageHandler = messageHandler;
    this.rootWidget = null;
    // Set up a few of our own properties
    this.env.proactive_context = [];
    this.env.engine = this;
    this.env.pushProactiveContext = function(p) { this.proactive_context.push(p); }.bind(this.env);
    this.env.popProactiveContext = function(p) { this.proactive_context.pop(); }.bind(this.env);

    install_foreign();
    this.baseURL = baseURL;
    if (this.baseURL.substring(0, 5).toLowerCase() == "https")
    {
        this.goalURI = "wss" + this.baseURL.substring(5) + "/goal";
        this.listenURI = "wss" + this.baseURL.substring(5) + "/listen";
    }
    else
    {
        this.goalURI = "ws" + this.baseURL.substring(4) + "/goal";
        this.listenURI = "ws" + this.baseURL.substring(4) + "/listen";
    }
    this.abortURI = this.baseURL + "/abort";
    this.componentURL = baseURL + "/component/";
    this.rootElementId = rootElementId;
    qOp = Prolog._create_options();
    Prolog._set_option(qOp, Prolog._make_atom("quoted"), Prolog._make_atom("true"));
    getServerConnection(this.listenURI, rootElementId, this.onMessage.bind(this), onConnectionLoss);
    this.make(callback);
}


function getServerConnection(URI, rootElementId, callback, onConnectionLoss)
{
    server_connection = new WebSocket(URI);
    server_connection.onmessage = callback;
    server_connection.onerror = function(event)
    {
        console.log("Warning: WS error: " + event);
        server_connection.close();
        window.setTimeout(function() { getServerConnection(URI, rootElementId, callback, onConnectionLoss); }, 5000);
    }
    server_connection.onclose = function(why)
    {
        console.log("Connection is closed!");
        if (pinger != null)
            clearInterval(pinger);
        pinger = null;
        console.log(why);
        console.log(new Error().stack);
        if (onConnectionLoss != undefined)
            onConnectionLoss(function()
                             {
                                 getServerConnection(URI, rootElementId, callback, onConnectionLoss);
                             });

    }.bind(this);
    server_connection.onopen = function()
    {
        server_connection.send(Prolog._format_term(qOp, 1200, Prolog._make_atom(rootElementId)) + ".\n");
        if (pinger == null)
        {
            pinger = setInterval(function()
                                      {
                                          console.log("Sending ping");
                                          server_connection.send("ping");
                                      }, 60000);
        }
        console.log("Session [re-]established");
           }.bind(this);
}

PrologEngine.prototype.checkForMessageHandlers = function(widget, callback)
{
    if (!Prolog._exists_predicate(Prolog._make_atom(widget.elementId), onMessageFunctor))
    {
        callback();
        return;
    }
    var savePoint = Prolog._save_state();
    var result = Prolog._make_variable();
    var delta = Prolog._make_variable();
    var goal = Prolog._make_compound(updateMessageHandlersFunctor, [Prolog._make_atom(widget.elementId),
                                                                    widget.listeners,
                                                                    widget.state.blob,
                                                                    widget.props.blob,
                                                                    delta,
                                                                    result]);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        var newListeners;
                        if (success)
                        {
                            Prolog._free_local(widget.listeners);
                            widget.listeners = Prolog._make_local(Prolog._deref(result));
                            this.registerForMessages(delta);
                            Prolog._restore_state(savePoint);
                        }
                        else
                        {
                            var ex = Prolog._get_exception();
                            if (ex != 0)
                                console.log("findMessageHandlers/4 raised an error: " + Prolog._format_term(null, 1200, ex));
                            Prolog._restore_state(savePoint);
                        }
                        callback();
                    }.bind(this));
}

PrologEngine.prototype.registerForMessages = function(term)
{
    server_connection.send("listen_for(" + Prolog._format_term(qOp, 1200, term) + ").\n");
}

PrologEngine.prototype.sendMessage = function(term)
{
    server_connection.send("message(" + Prolog._format_term(qOp, 1200, term) + ").\n");
}

PrologEngine.prototype.make = function(callback)
{
    console.log("Calling make()");
    // First destroy the old user module entirely. Otherwise re-loading boilerplate will end up with 2 definitions of otherwise/0 etc
    Prolog._hard_reset();
    install_foreign();
    console.log("Loading boilerplate");
    Prolog._consult_string(fs.readFileSync(__dirname + '/boilerplate.pl', 'utf8'));
    console.log("Loading vdiff");
    Prolog._consult_string(fs.readFileSync(__dirname + '/vdiff.pl', 'utf8'));
    console.log("Loading " + this.componentURL + this.rootElementId);
    Prolog._consult_url(this.componentURL + this.rootElementId, callback);
    console.log("Reinstalling user-defined foreign predicates")
    for (var i = 0; i < user_foreign_predicates.length; i++)
        Prolog.define_foreign(user_foreign_predicates[i].name, user_foreign_predicates[i].fn);
}

PrologEngine.prototype.getInitialState = function(widget, component, props, callback)
{
    if (!Prolog._exists_predicate(Prolog._make_atom(component), getInitialStateFunctor))
    {
        callback(PrologState.emptyState);
        return;
    }
    var savePoint = Prolog._save_state();
    var replyTerm = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(getInitialStateFunctor, [props.blob, replyTerm]));
    this.env.pushProactiveContext(widget.blob);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.env.popProactiveContext();
                        if (success)
                        {
                            var state = new PrologState(Prolog._deref(replyTerm));
                            Prolog._restore_state(savePoint);
                            //console.log("getInitialState has succeeded: " + state);
                            callback(state);
                        }
                        else
                        {
                            Prolog._restore_state(savePoint);
                            callback(PrologState.emptyState);
                        }
                    }.bind(this));
}

PrologEngine.prototype.componentWillReceiveProps = function(component, context, callback)
{
    if (!Prolog._exists_predicate(Prolog._make_atom(component), componentWillReceivePropsFunctor))
    {
        callback(false);
        return;
    }
    var savePoint = Prolog._save_state();
    var state = context.getState();
    var props = context.getProps();
    var newState = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(componentWillReceivePropsFunctor, [state.blob, props.blob, newState]));
    Prolog._execute(this.env,
                    goal, function(result)
                    {
                        if (result == 1)
                        {
                            var ss = context.getState().cloneWith(Prolog._deref(newState));
                            Prolog._restore_state(savePoint);
                            context.setState(ss, function() { callback(true)});
                        }
                        else
                        {
                            if (result == 2)
                                console.log("componentWillReceiveProps raised an error");
                            Prolog._restore_state(savePoint);
                            callback(false);
                        }
                    }.bind(this));
}

PrologEngine.prototype.render = function(widget, component, state, props, callback)
{
    var savePoint = Prolog._save_state();
    var vDom = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(renderFunctor, [state.blob, props.blob, vDom]));
    this.env.pushProactiveContext(widget.blob);
    this.rendering = true;
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.rendering = false;
                        this.env.popProactiveContext();
                        if (success)
                        {
                            // Expand child objects here
                            var expandedDom = Prolog._make_variable();
                            // Prolog._cut();
                            Prolog._execute(this.env,
                                            Prolog._make_compound(expandChildrenFunctor, [vDom, expandedDom]),
                                            function(s2)
                                            {
                                                vDom = Prolog._make_local(expandedDom);
                                                Prolog._restore_state(savePoint);
                                                if (s2)
                                                {
                                                    callback(vDom);
                                                }
                                                else
                                                {
                                                    var ex = Prolog._get_exception();
                                                    if (ex != 0)
                                                        console.log("expand_children/2 raised an error: "  + Prolog._format_term(null, 1200, ex));
                                                    else
                                                        console.log("expand_children/2 failed");
                                                    throw new Error("Stop: Render fail");
                                                }
                                            }
                                           );
                        }
                        else
                        {
                            Prolog._restore_state(savePoint);
                            var ex = Prolog._get_exception();
                            if (ex != 0)
                                console.log("render/3 raised an error: "  + Prolog._format_term(null, 1200, ex));
                            else
                                console.log("render/3 failed");
                            throw new Error("Stop: Render fail");
                            callback(Constants.emptyListAtom)
                        }
                    }.bind(this))
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context, callback)
{
    var savePoint = Prolog._save_state();
    var resultValue = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [context.blob]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        var element = null;
                        if (success)
                            element = Prolog._get_blob("react_component", resultValue);
                        else
                        {
                            var ex = Prolog._get_exception();
                            if (ex != 0)
                                console.log("createElementFromVDomFunctor Raised:" + Prolog._format_term(null, 1200, ex));
                            else
                                console.log("createElementFromVDomFunctor did not succeed");
                        }
                        Prolog._restore_state(savePoint);
                        callback(element);
                    }.bind(this));
}

PrologEngine.prototype.checkForFluxListeners = function(context)
{
    // FIXME: implement
}

PrologEngine.prototype.queueEvent = function(handler, event, context, callback)
{
    //console.log(Prolog._portray(handler));
    this.withEventQueue("queue",
                        function(then)
                        {
                            this.processEvent(handler, event, context, function(t)
                                              {
                                                  Prolog._free_local(handler);
                                                  Prolog._free_local(event);
                                                  callback(t);
                                                  then();}.bind(this));
                        }.bind(this));
}

PrologEngine.prototype.triggerEvent = function(handler, event, context, callback)
{
    this.withEventQueue("user",
                        function(then)
                        {
                            this.processEvent(handler, event, context, function(t) { callback(t); then();}.bind(this));
                        }.bind(this));
    this.processEvents();
}


PrologEngine.prototype.renderAuxComponent = function(handler, event, context, callback)
{
    var state, props;
    while (Prolog._is_compound(handler) && Prolog._term_functor(handler) == Constants.thisFunctor)
    {
        context = Prolog._get_blob("react_component", Prolog._term_arg(handler, 0));
        handler = Prolog._term_arg(handler, 1);
    }
    var savePoint = Prolog._save_state();
    var vDom = Prolog._make_variable();
    state = context.getState();
    props = context.getProps();
    var goal;
    if (Prolog._is_atom(handler))
    {
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(handler, [event, state.blob, props.blob, vDom]));
    }
    else if (Prolog._is_compound(handler))
    {
        var functor = Prolog._term_functor(handler);
        var arity = Prolog._term_functor_arity(handler);
        var args = [];
        var i;
        for (i = 0 ; i < arity; i++)
            args[i] = Prolog._term_arg(handler, i);
        args[i++] = event;
        args[i++] = state.blob;
        args[i++] = props.blob;
        args[i++] = vDom;
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(Prolog._term_functor_name(handler), args));
    }
    else
    {
        console.log("Invalid handler: " + handler);
        Prolog._restore_state(savePoint);
        callback(false);
        return;
    }
    this.env.pushProactiveContext(context.blob);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.env.popProactiveContext();
                        if (success)
                        {
                            // Expand child objects
                            var expandedDom = Prolog._make_variable();
                            Prolog._execute(this.env,
                                            Prolog._make_compound(expandChildrenFunctor, [vDom, expandedDom]),
                                            function(s2)
                                            {
                                                vDom = Prolog._make_local(expandedDom);
                                                Prolog._restore_state(savePoint);
                                                if (s2)
                                                {
                                                    callback(vDom);
                                                }
                                                else
                                                {
                                                    var ex = Prolog._get_exception();
                                                    if (ex != 0)
                                                        console.log("expand_children/2 raised an error: "  + Prolog._format_term(null, 1200, ex));
                                                    else
                                                        console.log("expand_children/2 failed");
                                                }
                                            });
                        }
                        else
                        {
                            Prolog._restore_state(savePoint);
                            var ex = Prolog._get_exception();
                            Prolog._clear_exception();
                            if (ex != 0)
                            {
                                if (this.errorHandler != undefined)
                                    this.errorHandler(ex);
                                console.log("render_aux_component/3 raised an error: "+ Prolog._format_term(null, 1200, ex));
                            }
                        }
                    }.bind(this));
}

PrologEngine.prototype.triggerTest = function(handler, event, context, callback)
{
    this.processTest(handler,
                     event,
                     context,
                     callback);
}

PrologEngine.prototype.triggerSystemEvent = function(handler, event, context, callback)
{
    this.withEventQueue("system",
                        function(then)
                        {
                            this.processEvent(handler, event, context, function(t) { callback(t); then();}.bind(this));
                        }.bind(this));
    this.processEvents();
}

PrologEngine.prototype.withEventQueue = function(origin, fn)
{
    this.event_queue.push(fn);
}

PrologEngine.prototype.processEvents = function()
{
    if (this.event_queue.length > 0 && this.busy == 0 && this.processing_event == 0)
    {
        this.processing_event = 1;
        this.event_queue.shift()(function(t)
                                 {
                                     this.processing_event = 0;
                                     this.processEvents();
                                 }.bind(this));
    }
    else if (this.busy > 0)
    {
        //console.log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Busy. Will do that in a moment");
    }
    else if (this.processing_event > 0)
    {
        //console.log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Already processing an event. Will do that one in a moment");
    }
    else
    {
        //console.log("All events are now dispatched");
    }
}

PrologEngine.prototype.processTest = function(handler, event, context, callback)
{
    var state, props;
    while (Prolog._is_compound(handler) && Prolog._term_functor(handler) == Constants.thisFunctor)
    {
        context = Prolog._get_blob("react_component", Prolog._term_arg(handler, 0));
        handler = Prolog._term_arg(handler, 1);
    }
    var savePoint = Prolog._save_state();
    state = context.getState();
    props = context.getProps();
    var goal;
    if (Prolog._is_atom(handler))
    {
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(handler, [event, state.blob, props.blob]));
    }
    else if (Prolog._is_compound(handler))
    {
        var functor = Prolog._term_functor(handler);
        var arity = Prolog._term_functor_arity(handler);
        var args = [];
        var i;
        for (i = 0 ; i < arity; i++)
            args[i] = Prolog._term_arg(handler, i);
        args[i++] = event;
        args[i++] = state.blob;
        args[i++] = props.blob;
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(Prolog._term_functor_name(handler), args));
    }
    else
    {
        console.log("Invalid handler: " + handler);
        Prolog._restore_state(savePoint);
        callback(false);
        return;
    }
    this.env.pushProactiveContext(context.blob);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.env.popProactiveContext();
                        Prolog._restore_state(savePoint);
                        var ex = Prolog._get_exception();
                        if (ex != 0)
                        {
                            if (this.errorHandler != undefined)
                                this.errorHandler(ex);
                            console.log("trigger_test/3 raised an error: "+ Prolog._format_term(null, 1200, ex));
                        }
                        callback(success);
                    }.bind(this));
}


PrologEngine.prototype.processEvent = function(handler, event, context, callback)
{
    var state, props;
    while (Prolog._is_compound(handler) && Prolog._term_functor(handler) == Constants.thisFunctor)
    {
        context = Prolog._get_blob("react_component", Prolog._term_arg(handler, 0));
        handler = Prolog._term_arg(handler, 1);
    }
    var savePoint = Prolog._save_state();
    state = context.getState();
    props = context.getProps();
    var goal;
    var newState = Prolog._make_variable();
    if (Prolog._is_atom(handler))
    {
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(handler, [event, state.blob, props.blob, newState]));
    }
    else if (Prolog._is_compound(handler))
    {
        var functor = Prolog._term_functor(handler);
        var arity = Prolog._term_functor_arity(handler);
        var args = [];
        var i;
        for (i = 0 ; i < arity; i++)
            args[i] = Prolog._term_arg(handler, i);
        args[i++] = event;
        args[i++] = state.blob;
        args[i++] = props.blob;
        args[i++] = newState;
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(Prolog._term_functor_name(handler), args));
    }
    else
    {
        console.log("Invalid handler: " + handler);
        Prolog._restore_state(savePoint);
        callback(false);
        return;
    }
    this.env.pushProactiveContext(context.blob);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.env.popProactiveContext();
                         var ss = null;
                         if (success)
                             ss = context.getState().cloneWith(Prolog._deref(newState))
                        Prolog._restore_state(savePoint);
                        if (success)
                        {
                            context.setState(ss, function()
                                             {
                                                 callback(true);
                                             }.bind(this));
                        }
                        else
                        {
                            var ex = Prolog._get_exception();
                            if (ex != 0)
                            {
                                if (this.errorHandler != undefined)
                                    this.errorHandler(ex);
                                console.log("trigger_event/4 raised an error: "+ Prolog._format_term(null, 1200, ex));
                                Prolog._clear_exception();
                            }
                            else
                                console.log("trigger_event/4 failed");
                            callback(false);
                        }
                     }.bind(this));
}

PrologEngine.prototype.mutate = function(a, b, root, callback)
{
    var savePoint = Prolog._save_state();
    var newRoot = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [root.getOwnerDocument().blob]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vMutateFunctor, [a, b, root.blob, renderOptions, newRoot]));
    Prolog._execute(this.env,
                    goal,
                    function(result)
                    {
                        var element = null;
                        if (result == 1)
                            element = Prolog._get_blob("react_component", newRoot);
                        Prolog._restore_state(savePoint);
                        if (result == 1)
                            callback(element);
                        /* Do not call the callback if we did not succeed */
                        else if (result == 2)
                            console.log("mutate/5 raised an error");
                        else if (result == 0)
                            console.log("mutate/5 failed");
                    }.bind(this));

}

PrologEngine.prototype.diff = function(a, b, callback)
{
    var savePoint = Prolog._save_state();
    var patchTerm = Prolog._make_variable();
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vDiffFunctor, [a, b, patchTerm]));
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        if (success)
                        {
                            var copy = Prolog._make_local(patchTerm);
                            Prolog._restore_state(savePoint);
                            callback(copy);
                            Prolog._free_local(copy);
                        }
                        else
                        {
                            Prolog._restore_state(savePoint);
                             var ex = Prolog._get_exception();
                             if (ex != 0)
                                 console.log("vdiff/3 raised an error: " + Prolog._format_term(null, 1200, ex));
                             else
                                 console.log("vdiff/3 failed");
                         }
                    }.bind(this));
}

PrologEngine.prototype.applyPatch = function(patch, root, callback)
{
    var savePoint = Prolog._save_state();
    var newRoot = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [root.getOwnerDocument().blob]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vPatchFunctor, [root.blob, patch, renderOptions, newRoot]));
    Prolog._execute(this.env,
                    goal,
                    function(result)
                    {
                        var element = null;
                        if (result == 1)
                            element = Prolog._get_blob("react_component", newRoot);
                        Prolog._restore_state(savePoint);
                        if (result == 1)
                            callback(element);
                        /* Do not call the callback if we did not succeed */
                        else if (result == 2)
                            console.log("applyPatch/4 raised an error");
                    }.bind(this));
}

// Messaging stuff is below
PrologEngine.prototype.setRootWidget = function(w)
{
    this.rootWidget = w;
}

var widgets = {};

PrologEngine.prototype.registerWidget = function(w)
{
    if (widgets[w.elementId] == undefined)
        widgets[w.elementId] = [w];
    else
        widgets[w.elementId].push(w);
}

PrologEngine.prototype.deregisterWidget = function(w)
{
    var index = widgets[w.elementId].indexOf(w);
    widgets[w.elementId].splice(index, 1);
}

PrologEngine.prototype.onMessage = function(event)
{
    // First read the term out of the message
    console.log("Got message: " + event.data);
    var t = Prolog._string_to_local_term(event.data);
    if (t == 0)
        console.log("Failed to parse message: " + event.data);
    else if (Prolog._is_compound(t))
    {
        if (Prolog._term_functor(t) == systemFunctor)
        {
            var v = Prolog._term_arg(t, 0);
            if (Prolog._is_compound(v) && Prolog._term_functor(v) == consultedFunctor)
            {
                Prolog._free_local(t);
                this.withEventQueue("internal",
                                    function(then)
                                    {
                                        this.make(function()
                                                  {
                                                      this.rootWidget.reRender(function() { console.log("Rebuilt due to code change on server"); });
                                                      then();
                                                  }.bind(this));
                                    }.bind(this));
                this.processEvents();
            }
            else
            {
                if (this.messageHandler != undefined)
                    this.messageHandler(v);
                else
                    console.log("System message received but no message handler has been registered");
                Prolog._free_local(t);
            }
        }
        else if (Prolog._term_functor(t) == messageFunctor)
        {
            // We know the modules that the server thinks should process this event
            var module = Prolog._atom_chars(Prolog._term_arg(t, 0));
            dispatchMessages(widgets[module], 0, t);
        }
        else
        {
            console.log("Unexpected message format1: " + Prolog._portray(t) + " from " + event.data);
            Prolog._free_local(t);
        }
    }
    else if (t == pongAtom)
    {
        console.log("Received pong");
    }
    else
    {
        console.log("Unexpected message format2: " + Prolog._portray(t) + " from " + event.data);
        Prolog._free_local(t);
    }

}

function dispatchMessages(w, i, t)
{
    if (w != undefined && i < w.length)
    {
        // For each of these trigger an event
        var handler = Prolog._make_local(Prolog._term_arg(t, 1));
        var event = Prolog._make_local(Prolog._term_arg(t, 2));
        w[i].triggerEvent(handler,
                          event,
                          function()
                          {
                              Prolog._free_local(handler);
                              Prolog._free_local(event);
                              dispatchMessages(w, i+1, t);
                          });
    }
    else
        Prolog._free_local(t);
}

PrologEngine.prototype.indicateBusy = function()
{
    this.busy++;
    var spinner = document.getElementById("$spinner");
    if (spinner !== null)
    {
        spinner.className = "busy";
        spinner.focus();
    }
}

PrologEngine.prototype.indicateReady = function()
{
    this.busy--;
    if (this.busy == 0)
    {
        var spinner = document.getElementById("$spinner");
        if (spinner !== null)
            spinner.className = "free";
    }
}
var user_foreign_predicates = [];
PrologEngine.registerPredicate = function(name, fn)
{
    user_foreign_predicates.push({name:name,
                                  fn:fn});
    Prolog.define_foreign(name, fn);
}

PrologEngine.prototype.abortCurrentServerGoal = function()
{
    var xhr = new XMLHttpRequest();
    xhr.open('get', this.abortURI + '?id=' + this.currentServerGoal);
    xhr.send();
}

module.exports = PrologEngine;
