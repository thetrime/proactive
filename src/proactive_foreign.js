"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var PrologState = require('./prolog_state');

var gluableAtom = new Prolog.AtomTerm("gluable");
var colonFunctor = new Prolog.Functor(new Prolog.AtomTerm(":"), 2);
var thisFunctor = new Prolog.Functor(new Prolog.AtomTerm("$this"), 2);

function crossModuleCall(module, goal)
{
    return new Prolog.CompoundTerm(Prolog.Constants.crossModuleCallFunctor, [new Prolog.AtomTerm(module), goal]);
}

function isNull(t)
{
    return (t instanceof Prolog.CompoundTerm && t.functor.equals(Prolog.Constants.curlyFunctor) && t.args[0].equals(nullAtom));
}

function addArgs(goal, glueArgs)
{
    if (goal instanceof Prolog.AtomTerm)
        return new Prolog.CompoundTerm(goal, glueArgs);
    else if (goal instanceof Prolog.CompoundTerm)
        return new Prolog.CompoundTerm(goal.functor.name, goal.args.concat(glueArgs));
    Prolog.Errors.typeError(Prolog.Constants.callableAtom, goal);
}

module.exports["."] = function(state, key, value)
{
    if (isNull(state))
        return this.unify(value, PrologState.nullTerm);
    if (!(state instanceof PrologState))
        Prolog.Errors.typeError(PrologState.prologStateAtom, state);
    if (key instanceof Prolog.AtomTerm)
        return this.unify(value, state.get(key));
    if (key instanceof Prolog.CompoundTerm)
    {
        var term = key;
        var glueArgs = term.args;
        var result = state.get(term.functor);
        if (isNull(result))
            return this.unify(value, result);
        if (result instanceof Prolog.CompoundTerm)
        {
            if (result.functor.equals(thisFunctor))
            {
                if (key.args[1] instanceof CompoundTerm && key.args[1].functor.equals(colonFunctor))
                {
                    var module = key.args[1].args[0];
                    var goal = key.args[1].args[1];
                    var newGoal = addArgs(goal, glueArgs);
                    return this.unify(value, new Prolog.CompoundTerm(result.functor, [term.args[0], new Prolog.CompoundTerm(Prolog.Constants.crossModuleCallFunctor, [module, newGoal])]));
                }
                else
                {
                    // No module
                    var newGoal = addArgs(term.args[1], glueArgs);
                    return this.unify(value, new Prolog.CompoundTerm(result.functor, [term.args[0], newGoal]));
                }
            }
            Prolog.Errors.typeError(gluableAtom, term);
        }
    }
    Prolog.Errors.typeError(PrologState.prologStateKeyAtom, state);
}

