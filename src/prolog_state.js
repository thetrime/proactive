"use strict";

var Errors = require('./errors.js');
var Constants = require('./constants.js');
var Prolog = require('../lib/proscript2/build/proscript.js');

// NB:
// A PrologState is basically a map of Javascript strings (keys) to PROLOG OBJECTS.


function make_null()
{
    return Prolog._make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
}

var util = require('util');

var global_state_id = 0;

function PrologState(t)
{
    this.id = global_state_id++;
    this.blob = Prolog._make_blob("state", this);
    this.map = {};
    if (arguments.length > 0)
        this.processElements(t);
}

PrologState.prototype.processElements = function(t)
{
    if (Prolog._is_variable(t))
        return;
    else if (t == Constants.curlyAtom)
        return;
    if (Prolog._is_compound(t) && Prolog._term_functor(t) == Constants.curlyFunctor)
    {
        var list = Prolog._term_arg(t, 0);
        while (Prolog._is_compound(list) && Prolog._term_functor(list) == Constants.commaFunctor)
        {
            var head = Prolog._term_arg(list, 0);
            this.processElement(head, Constants.colonFunctor);
            list = Prolog._term_arg(list, 1);
        }
        this.processElement(list, Constants.colonFunctor);
        return;
    }
    // FIXME: use real error?
    console.log(t);
    console.log(Prolog._format_term(null, 1200, t));
    throw new Error("Bad state 1");
}

PrologState.prototype.processElement = function(t, functor)
{
    if (Prolog._is_compound(t) && Prolog._term_functor(t) == functor)
    {
        var key = Prolog._term_arg(t, 0);
        var value = Prolog._term_arg(t, 1);
        if (!Prolog._is_atom(key))
            return Errors.typeError(Constants.atomAtom, key);
        this.processKeyPair(Prolog._atom_chars(key), value, functor);
    }
    else
    {
        console.log(t.toString());
        throw("Bad state 2");
    }
}

PrologState.prototype.portray = function(options, precedence)
{
    var output = "[";
    var keys = Object.keys(this.map);
    for (var i = 0 ; i < keys.length; i++)
    {
        output += Prolog._format_term(options, precedence, Prolog._make_compound(Constants.equalsFunctor, [Prolog._make_atom(keys[i]), this.map[keys[i]]]));
        if (i+1 < keys.length)
            output += ",";
    }
    return output + "]";
}

PrologState.prototype.toString = function()
{
    var mapinfo = "["
    var keys = Object.keys(this.map);
    for (var i = 0 ; i < keys.length; i++)
    {
        mapinfo += Prolog._format_term(null, 1200, Prolog._make_compound(Constants.equalsFunctor, [Prolog._make_atom(keys[i]), this.map[keys[i]]]));
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
    return (Prolog._is_compound(t) && Prolog._term_functor(t) == Constants.curlyFunctor) && (Prolog._term_arg(t, 0) != Constants.nullAtom);
}

function reify(t)
{
    if (Prolog._is_constant(t) || Prolog._is_variable(t))
        return t;
    if (Prolog._is_compound(t))
    {
        return Prolog._make_local(t);
    }
}

PrologState.prototype.freeState = function()
{
    var keys = Object.keys(this.map);
    for (var i = 0; i < keys.length; i++)
    {
        var value = this.map[keys[i]];
        if (Prolog._is_compound(value))
            Prolog._free_local(value);
        else if (Prolog._is_blob(value, "state"))
            Prolog._get_blob(value, "state").freeState();
    }
}

// key should be a Javascript string
PrologState.prototype.processKeyPair = function(key, value, functor)
{
    var existingValue = this.map[key];
    if (existingValue === undefined)
    {
        if (isState(value))
            this.map[key] = new PrologState(value).blob;
        else if (Prolog._is_variable(value))
            this.map[key] = make_null();
        else
        {
            this.map[key] = reify(value);
        }
    }
    else
    {
        if (Prolog._is_blob(existingValue, "state"))
        {
            existingValue = Prolog._get_blob(existingValue, "state");
            if (isState(value))
            {
                // Merge
                this.map[key] = existingValue.cloneWith(value);
            }
            else
            {
                existingValue.freeState();
                // Changing {foo: {bar: .....}} -> {foo: atomic-type}
                if (Prolog._is_variable(value))
                    this.map[key] = make_null();
                else
                    this.map[key] = reify(value);
            }
        }
        else
        {
            if (Prolog._is_compound(existingValue))
            {
                Prolog._free_local(existingValue);
            }
            if (isState(value))
                this.map[key] = new PrologState(value).blob;
            else if (Prolog._is_variable(value))
                this.map[key] = make_null();
            else
            {
                this.map[key] = reify(value);
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
    if (Prolog._is_blob(t, "state"))
    {
        t = Prolog._get_blob("state", t);
        keys = Object.keys(t.map);
        for (var i = 0; i < keys.length; i++)
            newState.processElement(keys[i], t.map[keys[i]], Constants.colonFunctor);
    }
    else
        newState.processElements(t);
    return newState;
}

// Expects key to be an AtomTerm
PrologState.prototype.get = function(key)
{
    return this.map[Prolog._atom_chars(key)] || make_null();
}

PrologState.fromList = function(t)
{
    // This is used for parsing the attributes list into a state
    var prologState = new PrologState();
    while (Prolog._is_compound(t))
    {
        if (Prolog._term_functor(t) != Constants.listFunctor)
            Errors.typeError(Constants.listAtom, t);
        else
            prologState.processElement(Prolog._term_arg(t, 0), Constants.equalsFunctor);
        t = Prolog._term_arg(t, 1);
    }
    return prologState;
}


PrologState.emptyState = new PrologState();

module.exports = PrologState;
