"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');

var colonFunctor = new Prolog.Functor(new Prolog.AtomTerm(":"), 2);
var equalsFunctor = new Prolog.Functor(new Prolog.AtomTerm("="), 2);
var commaFunctor = new Prolog.Functor(new Prolog.AtomTerm(","), 2);
var nullAtom = new Prolog.AtomTerm("null");
var nullTerm = new Prolog.CompoundTerm(Prolog.Constants.curlyFunctor, [nullAtom]);


function PrologState(t)
{
    this.map = {};
    this.processElements(t);
}

PrologState.prototype = new Prolog.AtomTerm;
PrologState.prototype.processElements = function(t)
{
    if (t instanceof Prolog.VariableTerm)
        return;
    else if (t.equals(Prolog.Constants.curlyAtom))
        return;
    if (t instanceof Prolog.CompoundTerm && t.functor.equals(Prolog.Constants.curlyFunctor))
    {
        var list = t.args[0].dereference();
        while (list instanceof Prolog.CompoundTerm && list.functor.equals(commaFunctor))
        {
            var head = list.args[0].dereference();
            this.processElement(head, colonFunctor);
            list = list.args[1].dereference();
        }
        this.processElement(list, colonFunctor);
        return;
    }
    // FIXME: use real error?
    console.log(t);
    throw("Bad state 1");
}

PrologState.prototype.processElement = function(t, functor)
{
    if (t instanceof Prolog.CompoundTerm && t.functor.equals(functor))
    {
        var key = t.args[0].dereference();
        var value = t.args[1].dereference();
        Prolog.Utils.must_be_atom(key);
        this.processKeyPair(key, value, functor);
    }
    else
    {
        console.log(t.toString());
        throw("Bad state 2");
    }
}

PrologState.prototype.toString = function()
{
    return "{prolog-state}";
}

function isState(t)
{
    return (t instanceof Prolog.CompoundTerm && t.functor.equals(Prolog.Constants.curlyFunctor) && !(t.args[0].equals(nullAtom)));
}

PrologState.prototype.processKeyPair = function(key, value, functor)
{
    var existingValue = this.map[key.value];
    if (existingValue === undefined)
    {
        if (isState(value))
            this.map[key.value] = new PrologState(value);
        else if (value instanceof Prolog.VariableTerm)
            this.map[key.value] = nullTerm;
        else
            this.map[key.value] = value.dereference_recursive();
    }
    else
    {
        existingValue = existingValue.dereference();
        if (existingValue instanceof PrologState)
        {
            if (isState(value))
            {
                // Merge
                this.map[key.value] = existingValue.cloneWith(value);
            }
            else
            {
               // Changing {foo: {bar: .....}} -> {foo: atomic-type}
               if (value instanceof Prolog.VariableTerm)
                   this.map[key.value] = nullTerm;
               else
                   this.map[key.value], value.dereference_recursive();
            }
        }
        else
        {
            if (isState(value))
                this.map[key.value] = new PrologState(value);
            else if (value instanceof Prolog.VariableTerm)
                this.map[key.value] = nullTerm;
            else
                this.map[key.value] = value.dereference_recursive();
        }
    }
}

PrologState.prototype.getClass = function()
{
    return "state";
}

PrologState.prototype.getProperties = function()
{
    return this.map;
}

// Expects key to be an AtomTerm
PrologState.prototype.get = function(key)
{
    return this.map[key.value] || nullTerm;
}

PrologState.fromList = function(t)
{
    // This is used for parsing the attributes list into a state
    var prologState = new PrologState(new Prolog.VariableTerm());
    while (t instanceof Prolog.CompoundTerm)
    {
        if (!t.functor.equals(Prolog.Constants.listFunctor))
            Prolog.Errors.typeError(Prolog.Constants.listAtom, t);
        else
            prologState.processElement(t.args[0].dereference(), equalsFunctor);
        t = t.args[1].dereference();
    }
    return prologState;
}

PrologState.emptyState = new PrologState(new Prolog.VariableTerm());
PrologState.nullTerm = nullTerm;
PrologState.prologStateAtom = new Prolog.AtomTerm("prolog_state");
PrologState.prologStateKeyAtom = new Prolog.AtomTerm("prolog_state_key");

module.exports = PrologState;
