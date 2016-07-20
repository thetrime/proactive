"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var ProactiveConstants = require('./proactive_constants');

var foreign_module = require('./proactive_foreign.js');

var getInitialStateFunctor = new Prolog.Functor(new Prolog.AtomTerm("getInitialState"), 2);
var componentWillReceiveProps = new Prolog.Functor(new Prolog.AtomTerm("componentWillReceiveProps"), 3);
var renderFunctor = new Prolog.Functor(new Prolog.AtomTerm("render"), 3);
var documentFunctor = new Prolog.Functor(new Prolog.AtomTerm("document"), 1);
var createElementFromVDomFunctor = new Prolog.Functor(new Prolog.AtomTerm("create_element_from_vdom"), 3);
var vDiffFunctor = new Prolog.Functor(new Prolog.AtomTerm("vdiff"), 3);
var vPatchFunctor = new Prolog.Functor(new Prolog.AtomTerm("vpatch"), 4);

function crossModuleCall(module, goal)
{
    return new Prolog.CompoundTerm(Prolog.Constants.crossModuleCallFunctor, [new Prolog.AtomTerm(module), goal]);
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
    var replyTerm = new Prolog.VariableTerm();
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(getInitialStateFunctor, [props, replyTerm]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var state = new PrologState(replyTerm.dereference_recursive());
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
                         console.log("getInitialState raised: " + error.toString());
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
    var newState = new Prolog.VariableTerm("NewState");
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(componentWillReceiveProps, [state, props, newState]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var ss = newState.dereference_recursive();
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
                         console.log("componentWillReceiveProps raised: " + error.toString());
                         this.env.restoreState(savePoint);
                         callback(false);
                     }.bind(this));
}

PrologEngine.prototype.render = function(widget, component, state, props, callback)
{
    var vDom = new Prolog.VariableTerm();
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(renderFunctor, [state, props, vDom]));
    var savePoint = this.env.saveState();
    this.env.pushProactiveContext(widget);
    this.env.execute(goal,
                     function()
                     {
                         this.env.popProactiveContext();
                         var result = vDom.dereference_recursive();
                         this.env.restoreState(savePoint);
                         callback(result);
                     }.bind(this),
                     function()
                     {
                         this.env.popProactiveContext();
                         this.env.restoreState(savePoint);
                         console.log(component + " render/3 failed");
                         callback(null);
                     }.bind(this),
                     function(error)
                     {
                         console.log("render/3 raised: " + error.toString());
                         this.env.popProactiveContext();
                         this.env.restoreState(savePoint);
                         callback(null);
                     }.bind(this));
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context, callback)
{
    var resultValue = new Prolog.VariableTerm();
    var renderOptions = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.CompoundTerm(documentFunctor, [new Prolog.BlobTerm("react_context", context)]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var result = resultValue.dereference().value;
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
                         console.log("createElementFromVDomFunctor raised: " + error.toString());
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
    while (handler instanceof Prolog.CompoundTerm && handler.functor.equals(ProactiveConstants.thisFunctor))
    {
        context = handler.args[0].value;
        handler = handler.args[1];
    }
    state = context.getState();
    props = context.getProps();
    var goal;
    var newState = new Prolog.VariableTerm("NewState");
    if (handler instanceof Prolog.AtomTerm)
        goal = crossModuleCall(context.getComponentName(), new Prolog.CompoundTerm(handler, [event, state, props, newState]));
    else if (handler instanceof Prolog.CompoundTerm)
        goal = crossModuleCall(context.getComponentName(), new Prolog.CompoundTerm(handler.functor.name, handler.args.concat([event, state, props, newState])));
    else
    {
        console.log("Invalid handler: " + handler);
        callback(false);
        return;
    }
    //console.log("Triggering in " + context.getComponentName() + " with props " + props.toString());
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var ss = context.getState().cloneWith(newState.dereference_recursive());;
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
                         //console.log("Event raised: " + error.toString());
                         this.env.restoreState(savePoint);
                         callback(error);
                     }.bind(this));
}

PrologEngine.prototype.diff = function(a, b, callback)
{
    var patchTerm = new Prolog.VariableTerm("Patch");
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(vDiffFunctor, [a, b, patchTerm]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var patch = patchTerm.dereference_recursive();
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
    var newRoot = new Prolog.VariableTerm("NewRoot");
    var renderOptions = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.CompoundTerm(documentFunctor, [new Prolog.BlobTerm("react_context", root.getOwnerDocument())]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(vPatchFunctor, [new Prolog.BlobTerm("react_component", root), patch, renderOptions, newRoot]));
    var savePoint = this.env.saveState();
    this.env.execute(goal,
                     function()
                     {
                         var value = newRoot.dereference().value;
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

module.exports = PrologEngine;
