"use strict";

var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var Constants = require('./constants');
var foreign_module = require('./proactive_foreign.js');
var Prolog = require('../lib/proscript2/build/proscript.js');

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
    this.env.pushProactiveContext = function(p) { this.proactive_context.push(p); };
    this.env.popProactiveContext = function(p) { this.proactive_context.pop(); };

    // Now load in the proactive foreign predicates
    var foreign_predicates = Object.keys(foreign_module);
    for (var p = 0; p < foreign_predicates.length; p++)
        Prolog.define_foreign(foreign_predicates[p], foreign_module[foreign_predicates[p]]);

    this.baseURL = baseURL;
    if (this.baseURL.substring(0, 5).toLowerCase() == "https")
        this.goalURI = "wss" + this.baseURL.substring(5) + "/goal";
    else
        this.goalURI = "ws" + this.baseURL.substring(4) + "/goal";
    this.listenURI = baseURL + "/listen";
    this.componentURL = baseURL + "/component/";
    this.rootElementId = rootElementId;
    this.make(callback);
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
    var goal = crossModuleCall(component, Prolog._make_compound(getInitialStateFunctor, [Prolog._make_blob("state", props), replyTerm]));
    var savePoint = Prolog._save_state();
    Prolog._execute(goal, function(result)
                    {
                        if (result == 1)
                        {
                            var state = new PrologState(replyTerm);
                            Prolog._restore_state(savePoint);
                            callback(state);
                        }
                        else
                        {
                            if (result == 2)
                                console.log("getInitialState raised an error");
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
    var goal = crossModuleCall(component, Prolog._make_compound(componentWillReceivePropsFunctor, [Prolog._make_blob("state", state),
                                                                                                   Prolog._make_blob("state", props),
                                                                                                   newState]));
    var savePoint = Prolog._save_state();
    Prolog._execute(goal, function(result)
                    {
                        if (result == 1)
                        {
                            var ss = context.getState().cloneWith(newState);
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
    var goal = crossModuleCall(component, Prolog._make_compound(renderFunctor, [Prolog._make_blob("state", state),
                                                                                Prolog._make_blob("state", props),
                                                                                vDom]));
    var savePoint = Prolog._save_state();
    this.env.pushProactiveContext(widget.blob);
    Prolog._execute(goal,
                    function(result)
                    {
                        this.env.popProactiveContext();
                        if (result == 1)
                            callback(vDom);
                        else
                        {
                            if (result == 2)
                                console.log("render/3 raised an error")
                            else
                                console.log("render/3 failed")
                            callback(Constants.emptyListAtom)
                        }
                        Prolog._restore_state(savePoint);
                    }.bind(this))
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context, callback)
{
    var resultValue = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [Prolog._make_blob("react_context", context)]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    var savePoint = Prolog._save_state();
    Prolog._execute(goal,
                    function(result)
                    {
                        var element = null;
                        if (result == 1)
                            element = _get_blob("dom_node", resultValue);
                        else
                        {
                            if (resultValue == 2)
                                console.log("createElementFromVDomFunctor raised an error");
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
    while (_is_compound(handler) && _term_functor(handler) == Constants.thisFunctor)
    {
        context = _get_blob("react_context", _term_arg(handler, 0));
        handler = _term_arg(handler, 1);
    }
    state = context.getState();
    props = context.getProps();
    var goal;
    var newState = Prolog._make_variable();
    if (_is_atom(handler))
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(handler, [event,
                                                                                           Prolog._make_blob("state", state),
                                                                                           Prolog._make_blob("state", props),
                                                                                           newState]));
    else if (_is_compound(handler))
    {
        var functor = _term_functor(handler);
        var arity = _term_functor_arity(handler);
        var args = [];
        var i;
        for (i = 0 ; i < arity; i++)
            args[i] = _term_arg(handler, i);
        args[i++] = event;
        args[i++] = Prolog._make_blob("state", state);
        args[i++] = Prolog._make_blob("state", props);
        args[i++] = newState;
        goal = crossModuleCall(context.getComponentName(), Prolog._make_compound(_term_functor_name(handler), args));
    }
    else
    {
        console.log("Invalid handler: " + handler);
        callback(false);
        return;
    }
    var savePoint = Prolog._save_state();
    this.env.execute(goal,
                     function(result)
                     {
                         var ss = null;
                         if (result == 1)
                         {
                             ss = context.getState().cloneWith(newState)
                         }
                         Prolog._restore_state(savePoint);
                         if (result == 1)
                             context.setState(ss, function() {callback(true);}.bind(this));
                         else
                         {
                             if (result == 0)
                             {
                                 callback(false);
                                 console.log("Event failed");
                             }
                             else if (result == 2)
                             {
                                 callback(_get_exception());
                                 console.log("Event raised an error");
                             }
                         }
                     }.bind(this));
}

PrologEngine.prototype.diff = function(a, b, callback)
{
    var patchTerm = Prolog._make_variable();
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vDiffFunctor, [a, b, patchTerm]));
    var savePoint = Prolog._save_state();
    Prolog._execute(goal,
                    function(result)
                    {
                        if (result == 1)
                        {
                            callback(patch);
                        }
                        else
                        {
                            if (result == 2)
                                console.log("diff/3 raised an error");
                            callback(Constants.emptyListAtom);
                        }
                        restore_state(savePoint);
                    }.bind(this));
}

PrologEngine.prototype.applyPatch = function(patch, root, callback)
{
    var newRoot = Prolog._make_variable();
    var renderOptions = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(documentFunctor, [Prolog._make_blob("react_context", root.getOwnerDocument())]), Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog._make_compound(vPatchFunctor, [Prolog._make_blob("react_component", root), patch, renderOptions, newRoot]));
    var savePoint = Prolog._save_state();
    this.env.execute(goal,
                     function(result)
                     {
                         var element = null;
                         if (result == 1)
                             element = _get_blob("dom_node", newRoot);
                         Prolog._restore_state(savePoint);
                         if (result == 1)
                             callback(element);
                         /* Do not call the callback if we did not succeed */
                         else if (result == 2)
                             console.log("applyPatch/4 raised an error");
                     }.bind(this));
}


module.exports = PrologEngine;
