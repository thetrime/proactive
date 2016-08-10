"use strict";

var Errors = require('./errors.js');
var Prolog = require('../lib/proscript2/build/proscript.js');

function make_null()
{
    return Prolog._make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
}

var util = require('util');

var global_state_id = 0;

function PrologState(t)
{
    this.id = global_state_id++;
    this.map = {};
    if (arguments.length > 0)
        this.processElements(t);
}

PrologState.prototype.processElements = function(t)
{
    if (_is_variable(t))
        return;
    else if (t == Constants.curlyAtom)
        return;
    if (_is_compound(t) && _term_functor(t) == Constants.curlyFunctor)
    {
        var list = _term_arg(t, 0);
        while (_is_compound(list) && _term_functor(list) == Constants.commaFunctor)
        {
            var head = _term_arg(list, 0);
            this.processElement(head, Constants.colonFunctor);
            list = _term_arg(list, 1);
        }
        this.processElement(list, Constants.colonFunctor);
        return;
    }
    // FIXME: use real error?
    console.log(t);
    throw("Bad state 1");
}

PrologState.prototype.processElement = function(t, functor)
{
    if (_is_compound(t) && _term_functor(t) == functor)
    {
        var key = _term_arg(t, 0);
        var value = _term_arg(t, 1);
        if (!_is_atom(key))
            return Errors.typeError(Constants.atomAtom, key);
        this.processKeyPair(_atom_chars(key), value, functor);
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
        output += Prolog.TermWriter.formatTerm(options, precedence, Prolog.CompoundTerm.create(Constants.equalsFunctor, [Prolog._make_atom(keys[i]), this.map[keys[i]]]));
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
        mapinfo += Prolog.TermWriter.formatTerm(options, 1200, Prolog.CompoundTerm.create(Constants.equalsFunctor, [Prolog._make_atom(keys[i]), this.map[keys[i]]]));
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
    return (_is_compound(t) && _term_functor(t) == Constants.curlyFunctor) && (_term_arg(t, 0) != nullAtom);
}

function reify(t)
{
    if (_is_constant(t) || _is_variable(t))
        return t;
    if (_is_compound(t))
    {
        return t;
        /*
        var args = [];
        var functor = Prolog.CTable.get(_term_functor(t));
        for (var i = 0; i < functor.arity; i++)
            args[i] = _term_arg(t, i);
        return {functor: _term_functor(t),
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
        else if (_is_variable(value))
            this.map[key.value] = nullTerm;
        else
        {
            this.map[key.value] = reify(value);
        }
    }
    else
    {
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
                if (_is_variable(value))
                    this.map[key.value] = nullTerm;
                else
                    this.map[key.value], reify(value);
            }
        }
        else
        {
            if (isState(value))
                this.map[key.value] = new PrologState(value);
            else if (_is_variable(value))
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
            newState.processElement(keys[i], t.map[keys[i]], Constants.colonFunctor);
    }
    else
        newState.processElements(t);
    return newState;
}

// Expects key to be an AtomTerm
PrologState.prototype.get = function(key)
{
    return this.map[key.value] || make_null();
}

PrologState.fromList = function(t)
{
    // This is used for parsing the attributes list into a state
    var prologState = new PrologState();
    while (_is_compound(t))
    {
        if (_term_functor(t) != Constants.listFunctor)
            Errors.typeError(Constants.listAtom, t);
        else
            prologState.processElement(_term_arg(t, 0), Constants.equalsFunctor);
        t = _term_arg(t, 1);
    }
    return prologState;
}


PrologState.emptyState = new PrologState();

module.exports = PrologState;
