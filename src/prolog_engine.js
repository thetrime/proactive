"use strict";

var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var Constants = require('./constants');
var foreign_module = require('./proactive_foreign.js');
var Prolog = require('proscript');

var getInitialStateFunctor = Prolog._make_functor(Prolog._make_atom("getInitialState"), 2);
var componentWillReceivePropsFunctor = Prolog._make_functor(Prolog._make_atom("componentWillReceiveProps"), 3);
var renderFunctor = Prolog._make_functor(Prolog._make_atom("render"), 3);
var documentFunctor = Prolog._make_functor(Prolog._make_atom("document"), 1);
var createElementFromVDomFunctor = Prolog._make_functor(Prolog._make_atom("create_element_from_vdom"), 3);
var vDiffFunctor = Prolog._make_functor(Prolog._make_atom("vdiff"), 3);
var vPatchFunctor = Prolog._make_functor(Prolog._make_atom("vpatch"), 4);

function crossModuleCall(module, goal)
{
    return Prolog._make_compound(Constants.crossModuleCallFunctor, [Prolog._make_atom(module), goal]);
}


function PrologEngine(baseURL, rootElementId, callback)
{
    this.env = {};
    // Set up a few of our own properties
    this.env.proactive_context = [];
    this.env.engine = this;
    this.env.pushProactiveContext = function(p) { this.proactive_context.push(p); }.bind(this.env);
    this.env.popProactiveContext = function(p) { this.proactive_context.pop(); }.bind(this.env);

    // Now load in the proactive foreign predicates
    var foreign_predicates = Object.keys(foreign_module);
    for (var p = 0; p < foreign_predicates.length; p++)
        Prolog.define_foreign(foreign_predicates[p], foreign_module[foreign_predicates[p]]);

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
    this.componentURL = baseURL + "/component/";
    this.rootElementId = rootElementId;
    getServerConnection(this.listenURI, this.onMessage.bind(this));
    this.make(callback);
}

function getServerConnection(URI, callback)
{
    var ws = new WebSocket(URI);
    ws.onmessage = callback;
    ws.onerror = function(event)
    {
        console.log("WS error: " + event);
        ws.close();
        getServerConnection(URI, callback);
    }
}

PrologEngine.prototype.make = function(callback)
{
    Prolog._consult_string(fs.readFileSync(__dirname + '/boilerplate.pl', 'utf8'));
    Prolog._consult_string(fs.readFileSync(__dirname + '/vdiff.pl', 'utf8'));
    console.log("Loading " + this.componentURL + this.rootElementId);
    Prolog._consult_url(this.componentURL + this.rootElementId, callback);
}

PrologEngine.prototype.getInitialState = function(component, props, callback)
{
    if (!Prolog._exists_predicate(Prolog._make_atom(component), getInitialStateFunctor))
    {
        callback(PrologState.emptyState);
        return;
    }
    var replyTerm = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(getInitialStateFunctor, [props.blob, replyTerm]));
    var savePoint = Prolog._save_state();
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
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
    var state = context.getState();
    var props = context.getProps();
    var newState = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(componentWillReceivePropsFunctor, [state.blob, props.blob, newState]));
    var savePoint = Prolog._save_state();
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
    var vDom = Prolog._make_variable();
    var goal = crossModuleCall(component, Prolog._make_compound(renderFunctor, [state.blob, props.blob, vDom]));
    var savePoint = Prolog._save_state();
    this.env.pushProactiveContext(widget.blob);
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
                        this.env.popProactiveContext();
                        vDom = Prolog._make_local(vDom);
                        Prolog._restore_state(savePoint);
                        if (success)
                        {
                            //console.log("Render of " + component + " succeeded!");
                            callback(vDom);
                        }
                        else
                        {
                            var ex = Prolog._get_exception();
                            if (ex != 0)
                                console.log("render/3 raised an error: " + ex  + Prolog._format_term(null, 1200, ex));
                            else
                                console.log("render/3 failed");
                            throw new Error("Stop: Render fail");
                            callback(Constants.emptyListAtom)
                        }
                    }.bind(this))
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context, callback)
{
    var resultValue = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [context.blob]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    var savePoint = Prolog._save_state();
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

PrologEngine.prototype.triggerEvent = function(handler, event, context, callback)
{
    var state, props;
    while (Prolog._is_compound(handler) && Prolog._term_functor(handler) == Constants.thisFunctor)
    {
        context = Prolog._get_blob("react_component", Prolog._term_arg(handler, 0));
        handler = Prolog._term_arg(handler, 1);
    }
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
        callback(false);
        return;
    }
    var savePoint = Prolog._save_state();
    Prolog._execute(this.env,
                    goal,
                    function(success)
                    {
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
                                console.log("trigger_event/4 raised an error: "+ Prolog._format_term(null, 1200, ex));
                            else
                                console.log("trigger_event/4 failed");
                        }
                     }.bind(this));
}

PrologEngine.prototype.diff = function(a, b, callback)
{
    var patchTerm = Prolog._make_variable();
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vDiffFunctor, [a, b, patchTerm]));
    var savePoint = Prolog._save_state();
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
    var newRoot = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [root.getOwnerDocument().blob]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vPatchFunctor, [root.blob, patch, renderOptions, newRoot]));
    var savePoint = Prolog._save_state();
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

var changeListeners = {};

PrologEngine.prototype.addCodeChangeListener = function(elementId, callback)
{
    if (changeListeners[elementId] === undefined)
        changeListeners[elementId] = [callback];
    else
        changeListeners[elementId].push(callback);
}

PrologEngine.prototype.onMessage = function(event)
{
    var callbacks = changeListeners[event.data];
    console.log("Code change: " + event.data);
    if (callbacks !== undefined)
    {
        this.make(function()
                  {
                      for (var i = 0; i < callbacks.length; i++)
                          callbacks[i]();
                  });
    }
}
module.exports = PrologEngine;
