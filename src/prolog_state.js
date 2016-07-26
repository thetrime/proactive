"use strict";

var Prolog = require('../lib/proscript2/src/core.js');

var colonFunctor = Prolog.Functor.get(Prolog.AtomTerm.get(":"), 2);
var equalsFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("="), 2);
var commaFunctor = Prolog.Functor.get(Prolog.AtomTerm.get(","), 2);
var nullAtom = Prolog.AtomTerm.get("null");
var nullTerm = Prolog.CompoundTerm.create(Prolog.Constants.curlyFunctor, [nullAtom]);

var util = require('util');

var global_state_id = 0;

function PrologState(t)
{
    this.id = global_state_id++;
    this.map = {};
    if (arguments.length > 0)
        this.processElements(t);
}

PrologState.prototype = new Prolog.AtomTerm();
PrologState.prototype.processElements = function(t)
{
    if (TAGOF(t) == VariableTag)
        return;
    else if (t == Prolog.Constants.curlyAtom)
        return;
    if (TAGOF(t) == CompoundTag && FUNCTOROF(t) == Prolog.Constants.curlyFunctor)
    {
        var list = ARGOF(t, 0);
        while (TAGOF(list) == CompoundTag && FUNCTOROF(list) == commaFunctor)
        {
            var head = ARGOF(list, 0);
            this.processElement(head, colonFunctor);
            list = ARGOF(list, 1);
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
    if (TAGOF(t) == CompoundTag && FUNCTOROF(t) == functor)
    {
        var key = ARGOF(t, 0);
        var value = ARGOF(t, 1);
        Prolog.Utils.must_be_atom(key);
        this.processKeyPair(Prolog.CTable.get(key), value, functor);
    }
    else
    {
        console.log(t.toString());
        throw("Bad state 2");
    }
}

PrologState.prototype.formatTerm = function(options, precedence)
{
    var output = "[";
    var keys = Object.keys(this.map);
    for (var i = 0 ; i < keys.length; i++)
    {
        output += Prolog.TermWriter.formatTerm(options, precedence, Prolog.CompoundTerm.create(equalsFunctor, [Prolog.AtomTerm.get(keys[i]), this.map[keys[i]]]));
        if (i+1 < keys.length)
            output += ",";
    }
    return output + "]";
}

PrologState.prototype.toString = function()
{
    var mapinfo = "["
    var options = {};
    var keys = Object.keys(this.map);
    for (var i = 0 ; i < keys.length; i++)
    {
        mapinfo += Prolog.TermWriter.formatTerm(options, 1200, Prolog.CompoundTerm.create(equalsFunctor, [Prolog.AtomTerm.get(keys[i]), this.map[keys[i]]]));
        if (i+1 < keys.length)
            mapinfo += ",";
    }
    return "{prolog-state: " + mapinfo + "]}";
}

PrologState.prototype.hashCode = function()
{
    return this.id;
}

function isState(t)
{
    return (TAGOF(t) == CompoundTag && FUNCTOROF(t) == Prolog.Constants.curlyFunctor) && (ARGOF(t, 0) != nullAtom);
}

function reify(t)
{
    t = DEREF(t);
    if (TAGOF(t) == ConstantTag || TAGOF(t) == VariableTag)
        return t;
    if (TAGOF(t) == CompoundTag)
    {
        return t;
        /*
        var args = [];
        var functor = Prolog.CTable.get(FUNCTOROF(t));
        for (var i = 0; i < functor.arity; i++)
            args[i] = ARGOF(t, i);
        return {functor: FUNCTOROF(t),
                args: args};
        */
    }
}

PrologState.prototype.processKeyPair = function(key, value, functor)
{
    var existingValue = this.map[key.value];
    if (existingValue === undefined)
    {
        if (isState(value))
            this.map[key.value] = new PrologState(value);
        else if (TAGOF(value) == VariableTag)
            this.map[key.value] = nullTerm;
        else
        {
            this.map[key.value] = reify(value);
        }
    }
    else
    {
        existingValue = DEREF(existingValue);
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
                if (TAGOF(value) == VariableTag)
                    this.map[key.value] = nullTerm;
                else
                    this.map[key.value], reify(value);
            }
        }
        else
        {
            if (isState(value))
                this.map[key.value] = new PrologState(value);
            else if (TAGOF(value) == VariableTag)
                this.map[key.value] = nullTerm;
            else
            {
                this.map[key.value] = reify(value);
            }
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

PrologState.prototype.cloneWith = function(t)
{
    var newState = new PrologState();
    var keys = Object.keys(this.map);
    for (var i = 0; i < keys.length; i++)
        newState.map[keys[i]] = this.map[keys[i]];
    if (t instanceof PrologState)
    {
        keys = Object.keys(t.map);
        for (var i = 0; i < keys.length; i++)
            newState.processElement(keys[i], t.map[keys[i]], colonFunctor);
    }
    else
        newState.processElements(t);
    return newState;
}

// Expects key to be an AtomTerm
PrologState.prototype.get = function(key)
{
    return this.map[key.value] || nullTerm;
}

PrologState.fromList = function(t)
{
    // This is used for parsing the attributes list into a state
    var prologState = new PrologState();
    while (TAGOF(t) == CompoundTag)
    {
        if (FUNCTOROF(t) != Prolog.Constants.listFunctor)
            Prolog.Errors.typeError(Prolog.Constants.listAtom, t);
        else
            prologState.processElement(ARGOF(t, 0), equalsFunctor);
        t = ARGOF(t, 1);
    }
    return prologState;
}


PrologState.emptyState = new PrologState();
PrologState.nullTerm = nullTerm;
PrologState.prologStateAtom = Prolog.AtomTerm.get("prolog_state");
PrologState.prologStateKeyAtom = Prolog.AtomTerm.get("prolog_state_key");

module.exports = PrologState;
