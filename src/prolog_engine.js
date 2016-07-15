"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var fs = require('fs');
var util = require('util');
var PrologState = require('./prolog_state');

var foreign_module = require('./proactive_foreign.js');

var getInitialStateFunctor = new Prolog.Functor(new Prolog.AtomTerm("getInitialState"), 2);
var renderFunctor = new Prolog.Functor(new Prolog.AtomTerm("render"), 3);
var documentFunctor = new Prolog.Functor(new Prolog.AtomTerm("document"), 1);
var createElementFromVDomFunctor = new Prolog.Functor(new Prolog.AtomTerm("create_element_from_vdom"), 3);

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

module.exports = PrologEngine;
