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
    this.env.pushContext = function(p) { this.proactive_context.push(p); };
    this.env.popContext = function(p) { this.proactive_context.pop(); };

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
    try
    {
        if (this.env.execute(goal))
        {
            var result = new PrologState(replyTerm.dereference());
            this.env.reset();
        }
        this.env.reset();
    }
    catch(e)
    {
        console.log(e);
    }
    return PrologState.emptyState;
}

PrologEngine.prototype.render = function(widget, component, state, props)
{
    var vDom = new Prolog.VariableTerm();
    console.log(state.toString());
    var goal = crossModuleCall(component, new Prolog.CompoundTerm(renderFunctor, [state, props, vDom]));
    this.env.pushContext(widget);
    try
    {
        console.log("Calling render:" + goal.toString());
        if (this.env.execute(goal));
        {
            console.log("rendered to: ");
            console.log(vDom);
            console.log(vDom.value === null);
            console.log(vDom.toString());
            console.log(util.inspect(vDom, {depth:null}));
            throw(0);
            return vDom.dereference_recursive();
        }
    }
    finally
    {
        this.env.popContext();
        this.env.reset();
    }
    return null;
}

PrologEngine.prototype.createElementFromVDom = function(vDOM, context)
{
    var resultValue = new Prolog.VariableTerm();
    var renderOptions = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.CompoundTerm(documentFunctor, [new Prolog.BlobTerm("react_context", context)]), Prolog.Constants.emptyListAtom]);
    var goal = crossModuleCall("vdiff", new Prolog.CompoundTerm(createElementFromVDomFunctor, [renderOptions, vDOM, resultValue]));
    try
    {
        if (this.env.execute(goal));
        {
            return vDom.dereference_recursive();
        }
    }
    finally
    {
        this.env.reset();
    }
    console.log("create_element_from_vdom/3 failed on " + vDOM);
    return null;
}

module.exports = PrologEngine;
