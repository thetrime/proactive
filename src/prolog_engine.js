"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');
var ProactiveConstants = require('./proactive_constants');

var foreign_module = require('./proactive_foreign.js');

var getInitialStateFunctor = new Prolog.Functor(new Prolog.AtomTerm("getInitialState"), 2);
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
    this.env.pushProactiveContext = function(p) { this.proactive_context.push(p); };
    this.env.popProactiveContext = function(p) { this.proactive_context.pop(); };

    // Now load in the proactive foreign predicates
    var foreign_predicates = Object.keys(foreign_module);
    for (var p = 0; p < foreign_predicates.length; p++)
        this.env.userModule.defineForeignPredicate(foreign_predicates[p], foreign_module[foreign_predicates[p]]);

    this.baseURL = baseURL;
    this.goalURI = baseURL + "/goal";
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

PrologEngine.prototype.getInitialState = function(component, props)
{
    var module = this.env.getModule(component);
    if (module === undefined || !module.predicateExists(getInitialStateFunctor))
        return PrologState.emptyState;
    var replyTerm = new Prolog.VariableTerm();
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(getInitialStateFunctor, [props, replyTerm]));
    var b = this.env.pushContext();
    try
    {
        if (this.env.execute(goal))
            return new PrologState(replyTerm.dereference());
    }
    catch(e)
    {
        console.log(e);
    }
    finally
    {
        this.env.popContext();
    }
    return PrologState.emptyState;
}

PrologEngine.prototype.render = function(widget, component, state, props)
{
    var vDom = new Prolog.VariableTerm();
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(renderFunctor, [state, props, vDom]));
    var b = this.env.pushContext();
    this.env.pushProactiveContext(widget);
    try
    {
        if (this.env.execute(goal))
        {
            return vDom.dereference_recursive();
        }
        console.log("render/3 failed");
    }
    finally
    {
        this.env.popProactiveContext();
        this.env.popContext();
    }
    return null;
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context)
{
    var resultValue = new Prolog.VariableTerm();
    var renderOptions = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.CompoundTerm(documentFunctor, [new Prolog.BlobTerm("react_context", context)]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    var b = this.env.pushContext();
    try
    {
        if (this.env.execute(goal))
        {
            return resultValue.dereference().value;
        }
    }
    finally
    {
        this.env.popContext(b);
    }
    return null;
}

PrologEngine.prototype.checkForFluxListeners = function(context)
{
    // FIXME: implement
}

PrologEngine.prototype.triggerEvent = function(handler, event, context)
{
    var state, props;
    if (handler instanceof Prolog.CompoundTerm && handler.functor.equals(ProactiveConstants.thisFunctor))
    {
        var target = handler.args[0].value;
        var newHandler = handler.args[1];
        return triggerEvent(newHandler, event, target);
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
        return false;
    }
    var b = this.env.pushContext();
    console.log("Raising event: " + goal.toString());
    try
    {
        if (this.env.execute(goal))
        {
            console.log("Event OK: " + goal.toString());
            context.setState(context.getState().cloneWith(newState.dereference()));
            console.log("Event OK2: " + goal.toString());
            return true;
        }
    }
    catch(error)
    {
        console.log(error);
    }
    finally
    {
        this.env.popContext(b);
    }
    return false;
}

PrologEngine.prototype.diff = function(a, b)
{
    var patchTerm = new Prolog.VariableTerm("Patch");
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(vDiffFunctor, [a, b, patchTerm]));
    this.env.pushContext();
    try
    {
        if (this.env.execute(goal))
        {
            console.log("OK: " + patchTerm.toString());
            return patchTerm.dereference_recursive();
        }
        else
            console.log("vdiff failed");
    }
    catch(error)
    {
        console.log("vdiff raised an error!");
        console.log(error.toString());
        console.log(error.stack);
    }
    finally
    {
        this.env.popContext();
    }
    return Prolog.Constants.emptyListAtom;
}

PrologEngine.prototype.applyPatch = function(patch, root)
{
    var newRoot = new Prolog.VariableTerm("NewRoot");
    var renderOptions = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.CompoundTerm(documentFunctor, [new Prolog.BlobTerm("react_context", root.getOwnerDocument())]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(vPatchFunctor, [new Prolog.BlobTerm("react_component", root), patch, renderOptions, newRoot]));
    var b = this.env.pushContext();
    console.log("About to vPatch: ");
    console.log(goal);
    try
    {
        if (this.env.execute(goal))
        {
            console.log("vPatch OK");
            return patchTerm.dereference_recursive();
        }
        else
            console.log("vPatch failed");
    }
    catch(error)
    {
        console.log("vPatch error");
        console.log(error.toString());
        console.log(error.stack);
    }
    finally
    {
        this.env.popContext(b);
    }
    console.log("*********************** patch/4 failed: " + patch.toString());
    throw new Error("Fatal error");
}

module.exports = PrologEngine;
