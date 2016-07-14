"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var PrologState = require('./prolog_state');
var ReactWidget = require('./react_widget');

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

/* First, Prolog-type stuff */
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


module.exports["state_to_term"] = function(state, term)
{
    throw new Error("FIXME: state_to_term not implemented");
}

module.exports["on_server"] = function(goal)
{
    throw new Error("FIXME: on_server not implemented");
}

module.exports["raise_event"] = function(a, b)
{
    throw new Error("FIXME: raise_event not implemented");
}

module.exports["wait_for"] = function(fluxion)
{
    throw new Error("FIXME: wait_for not implemented");
}

module.exports["get_this"] = function(t)
{
    throw new Error("FIXME: get_this not implemented");
}

module.exports["get_store_state"] = function(fluxion, state)
{
    throw new Error("FIXME: get_store_state not implemented");
}

module.exports["bubble_event"] = function(a, b)
{
    throw new Error("FIXME: bubble_event not implemented");
}

/* And now the DOM glue */
module.exports["remove_child"] = function(parent, child)
{
    throw new Error("FIXME: remove_child not implemented");
}

module.exports["append_child"] = function(parent, child)
{
    throw new Error("FIXME: append_child not implemented");
}

module.exports["insert_before"] = function(parent, child, sibling)
{
    throw new Error("FIXME: insert_before not implemented");
}

module.exports["replace_child"] = function(parent, newChild, oldChild)
{
    throw new Error("FIXME: replace_child not implemented");
}

module.exports["child_nodes"] = function(parent, children)
{
    throw new Error("FIXME: child_nodes not implemented");
}

module.exports["create_element"] = function(context, tagname, domnode)
{
    throw new Error("FIXME: create_element not implemented");
}

module.exports["create_text_node"] = function(context, text, domnode)
{
    throw new Error("FIXME: create_text_node not implemented");
}

module.exports["parent_node"] = function(node, parent)
{
    throw new Error("FIXME: parent_node not implemented");
}

module.exports["node_type"] = function(node, type)
{
    throw new Error("FIXME: node_type not implemented");
}

module.exports["set_vdom_properties"] = function(domNode, properties)
{
    throw new Error("FIXME: set_vdom_properties not implemented");
}

module.exports["replace_node_data"] = function(domNode, properties)
{
    throw new Error("FIXME: replace_node_data not implemented");
}

module.exports["destroy_widget"] = function(domNode)
{
    throw new Error("FIXME: destroy_widget not implemented");
}

module.exports["init_widget"] = function(context, properties, domNode)
{
    Prolog.Utils.must_be_blob("react_context", context);
    var parentContext = context.value;
    console.log(parentContext);
    console.log(properties.dereference_recursive().toString());
    var widget = new ReactWidget(parentContext, parentContext.getEngine(), element.args[0].value, PrologState.fromList(element.args[1]));
    return this.unify(domNode, new Prolog.BlobTerm("react_widget", widget));
}

module.exports["update_widget"] = function(newVDom, oldVDom, domNode, newDomNode)
{
    throw new Error("FIXME: update_widget not implemented");
}

