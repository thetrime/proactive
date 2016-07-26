"use strict";

var Prolog = require('../lib/proscript2/src/core.js');
var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var ProactiveConstants = require('./proactive_constants');
var foreign_module = require('./proactive_foreign.js');

var getInitialStateFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("getInitialState"), 2);
var componentWillReceiveProps = Prolog.Functor.get(Prolog.AtomTerm.get("componentWillReceiveProps"), 3);
var renderFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("render"), 3);
var documentFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("document"), 1);
var createElementFromVDomFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("create_element_from_vdom"), 3);
var vDiffFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("vdiff"), 3);
var vPatchFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("vpatch"), 4);

function crossModuleCall(module, goal)
{
    return Prolog.CompoundTerm.create(Prolog.Constants.crossModuleCallFunctor, [Prolog.AtomTerm.get(module), goal]);
}

function intern(t)
{
    return (Prolog.CTable.intern(t) | 0xc0000000) | 0;
}

function PrologEngine(baseURL, rootElementId, callback)
{
    this.env = new Prolog.Environment();
    // Set up a few of our own properties
    this.env.proactive_context = [];
    this.env.engine = this;
    this.env.pushProactiveContext = function(p) { this.proactive_context.push(p); };
    this.env.popProactiveContext = function(p) { this.proactive_context.pop(); };

    // Now load in the proactive foreign predicates
    var foreign_predicates = Object.keys(foreign_module);
    for (var p = 0; p < foreign_predicates.length; p++)
        this.env.userModule.defineForeignPredicate(foreign_predicates[p], foreign_module[foreign_predicates[p]]);

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
    this.env.consultString(fs.readFileSync(__dirname + '/boilerplate.pl', 'utf8'));
    this.env.consultString(fs.readFileSync(__dirname + '/vdiff.pl', 'utf8'));
    console.log("Loading " + this.componentURL + this.rootElementId);
    this.env.consultURL(this.componentURL + this.rootElementId, callback);
}

PrologEngine.prototype.getInitialState = function(component, props, callback)
{
    var module = this.env.getModule(component);
    if (module === undefined || !module.predicateExists(getInitialStateFunctor))
    {
        callback(PrologState.emptyState);
        return;
    }
    var replyTerm = Prolog.VariableTerm.create();
    var goal = crossModuleCall(component, Prolog.CompoundTerm.create(getInitialStateFunctor, [intern(props), replyTerm]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var state = new PrologState(DEREF(replyTerm));
                         this.env.restoreState(savePoint);
                         callback(state);
                     }.bind(this),
                     function()
                     {
                         this.env.restoreState(savePoint);
                         callback(PrologState.emptyState);
                     }.bind(this),
                     function(error)
                     {
                         console.log("getInitialState raised: " + PORTRAY(error));
                         this.env.restoreState(savePoint);
                         callback(PrologState.emptyState);
                     }.bind(this));
}

PrologEngine.prototype.componentWillReceiveProps = function(component, context, callback)
{
    var module = this.env.getModule(component);
    if (module === undefined || !module.predicateExists(componentWillReceiveProps))
    {
        callback(false);
        return;
    }
    var state = context.getState();
    var props = context.getProps();
    var newState = Prolog.VariableTerm.create("NewState");
    var goal = crossModuleCall(component, Prolog.CompoundTerm.create(componentWillReceiveProps, [intern(state),
                                                                                                 intern(props),
                                                                                                 newState]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var ss = DEREF(newState);
                         this.env.restoreState(savePoint);
                         context.setState(context.getState().cloneWith(ss), function() { callback(true)}.bind(this));
                     }.bind(this),
                     function()
                     {
                         this.env.restoreState(savePoint);
                         callback(false);
                     }.bind(this),
                     function(error)
                     {
                         console.log("componentWillReceiveProps raised: " + PORTRAY(error));
                         this.env.restoreState(savePoint);
                         callback(false);
                     }.bind(this));
}

PrologEngine.prototype.render = function(widget, component, state, props, callback)
{
    var vDom = Prolog.VariableTerm.create();
    var goal = crossModuleCall(component, Prolog.CompoundTerm.create(renderFunctor, [intern(state),
                                                                                     intern(props),
                                                                                     vDom]));
    var savePoint = this.env.saveState();
    this.env.pushProactiveContext(widget);
    this.env.execute(goal,
                     function()
                     {
                         this.env.popProactiveContext();
                         var result = DEREF(vDom);
                         this.env.restoreState(savePoint);
                         callback(result);
                     }.bind(this),
                     function()
                     {
                         this.env.popProactiveContext();
                         this.env.restoreState(savePoint);
                         console.log(component + " render/3 failed");
                         callback(Prolog.Constants.emptyListAtom);
                     }.bind(this),
                     function(error)
                     {
                         console.log("render/3 raised: " + PORTRAY(error));
                         this.env.popProactiveContext();
                         this.env.restoreState(savePoint);
                         callback(Prolog.Constants.emptyListAtom);
                     }.bind(this));
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context, callback)
{
    var resultValue = Prolog.VariableTerm.create();
    var renderOptions = Prolog.CompoundTerm.create(Prolog.Constants.listFunctor, [Prolog.CompoundTerm.create(documentFunctor, [Prolog.BlobTerm.get("react_context", context)]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog.CompoundTerm.create(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var result = Prolog.CTable.get(DEREF(resultValue)).value;
                         this.env.restoreState(savePoint);
                         callback(result);
                     }.bind(this),
                     function()
                     {
                         this.env.restoreState(savePoint);
                         callback(null);
                     }.bind(this),
                     function(error)
                     {
                         console.log("createElementFromVDomFunctor raised: " + PORTRAY(error))
                         this.env.restoreState(savePoint);
                         callback(null);
                     }.bind(this));
}

PrologEngine.prototype.checkForFluxListeners = function(context)
{
    // FIXME: implement
}

PrologEngine.prototype.triggerEvent = function(handler, event, context, callback)
{
    var state, props;
    while (TAGOF(handler) == CompoundTag && FUNCTOROF(handler) == ProactiveConstants.thisFunctor)
    {
        console.log("Handler refers to this");
        context = Prolog.CTable.get(ARGOF(handler, 0)).value;
        handler = ARGOF(handler, 1);
    }
    console.log("Final Handler: " + PORTRAY(handler));
    state = context.getState();
    props = context.getProps();
    var goal;
    var newState = Prolog.VariableTerm.create("NewState");
    if (TAGOF(handler) == ConstantTag && Prolog.CTable.get(handler) instanceof Prolog.AtomTerm)
        goal = crossModuleCall(context.getComponentName(), Prolog.CompoundTerm.create(handler, [event,
                                                                                                intern(state),
                                                                                                intern(props),
                                                                                                newState]));
    else if (TAGOF(handler) == CompoundTag)
    {
        var functor = Prolog.CTable.get(FUNCTOROF(handler));
        var args = [];
        var i;
        for (i = 0 ; i < functor.arity; i++)
            args[i] = ARGOF(handler, i);
        args[i++] = event;
        args[i++] = intern(state);
        args[i++] = intern(props);
        args[i++] = newState;

        goal = crossModuleCall(context.getComponentName(), Prolog.CompoundTerm.create(functor.name, args));
        console.log("Goal");
        console.log(PORTRAY(goal));
    }
    else
    {
        console.log("Invalid handler: " + handler);
        callback(false);
        return;
    }
    //console.log("Triggering in " + context.getComponentName() + " with props " + props.toString());
    //console.log(PORTRAY(goal));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var ss = context.getState().cloneWith(DEREF(newState))
                         this.env.restoreState(savePoint);
                         context.setState(ss, function() {callback(true);}.bind(this));
                     }.bind(this),
                     function()
                     {
                         console.log("Event failed");
                         this.env.restoreState(savePoint);
                         callback(false);
                     }.bind(this),
                     function(error)
                     {
                         console.log("Event raised: " + PORTRAY(error));
                         this.env.restoreState(savePoint);
                         callback(error);
                     }.bind(this));
}

PrologEngine.prototype.diff = function(a, b, callback)
{
    var patchTerm = Prolog.VariableTerm.create("Patch");
    var goal = crossModuleCall("vdiff", Prolog.CompoundTerm.create(vDiffFunctor, [a, b, patchTerm]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var patch = DEREF(patchTerm);
                         this.env.restoreState(savePoint);
                         callback(patch);
                     }.bind(this),
                     function()
                     {
                         this.env.restoreState(savePoint);
                         callback(Prolog.Constants.emptyListAtom);
                     }.bind(this),
                     function(error)
                     {
                         console.log("diff raised: " + error.toString());
                         this.env.restoreState(savePoint);
                         callback(Prolog.Constants.emptyListAtom);
                     }.bind(this));
}

PrologEngine.prototype.applyPatch = function(patch, root, callback)
{
    var newRoot = Prolog.VariableTerm.create("NewRoot");
    var renderOptions = Prolog.CompoundTerm.create(Prolog.Constants.listFunctor, [Prolog.CompoundTerm.create(documentFunctor, [Prolog.BlobTerm.get("react_context", root.getOwnerDocument())]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", Prolog.CompoundTerm.create(vPatchFunctor, [Prolog.BlobTerm.get("react_component", root), patch, renderOptions, newRoot]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var value = Prolog.CTable.get(DEREF(newRoot)).value;
                         this.env.restoreState(savePoint);
                         callback(value);
                     }.bind(this),
                     function()
                     {
                         this.env.restoreState(savePoint);
                         // Do not call callback
                     }.bind(this),
                     function(error)
                     {
                         console.log("applyPatch raised: " + error.toString());
                         this.env.restoreState(savePoint);
                         // Do not call callback
                     }.bind(this));
}

PrologEngine.prototype.debugStuff = function()
{
    console.log("Total instructions: " + this.env.debugger_steps);
    var k = Object.keys(this.env.debug_ops);
    for (var i = 0; i < k.length; i++)
    {
        var opcode = Prolog.Opcodes[k[i]].label;
        console.log("   " + opcode + ": " + this.env.debug_ops[k[i]] + " executions taking a total of " + this.env.debug_times[k[i]] + "ms");
        this.env.debug_ops[k[i]] = 0;
        this.env.debug_times[k[i]] = 0;
    }
    var k = Object.keys(this.env.debug_times);

    console.log("Backtracks: " + this.env.debug_backtracks);
    console.log("Max size of the trail: " + this.env.trail.length);

}

module.exports = PrologEngine;
