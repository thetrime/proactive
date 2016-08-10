"use strict";

var Constants = require('./constants.js');
var PrologState = require('./prolog_state');
var ReactWidget = require('./react_widget');
var ProactiveComponentFactory = require('./proactive_component_factory');
var Prolog = require('../lib/proscript2/build/proscript.js');

var util = require('util');


function crossModuleCall(module, goal)
{
    return Prolog._make_compound(Constants.crossModuleCallFunctor, [Prolog._make_atom(module), goal]);
}

function isNull(t)
{
    return (Prolog._is_compound(t) && _term_functor(t) == Constants.curlyFunctor && _term_arg(t, 0) == Constants.nullAtom);
}

function addArgs(goal, glueArgs)
{
    if (Prolog._is_atom(goal))
        return Prolog._make_compound(goal, glueArgs);
    else if (Prolog._is_compound(goal))
    {
        var args = [];
        var functor = _term_functor(goal);
        var arity = _term_functor_arity(goal);
        for (var i = 0; i < arity; i++)
            args[i] = _term_arg(goal,i);
        return Prolog._make_compound(_term_functor_name(goal), args.concat(glueArgs));
    }
    Errors.typeError(Constants.callableAtom, goal);
}

/* First, Prolog-type stuff */
module.exports["."] = function(state, key, value)
{
    if (isNull(state))
    {
        return _unify(value, Prolog._make_compound(Constants.curlyFunctor, [Constants.nullAtom]));
    }
    if (!Prolog._is_blob(state, "state"))
    {
        Errors.typeError(Constants.prologStateAtom, state);
    }
    if (Prolog._is_atom(key))
    {
        return _unify(value, _get_blob("state", state).get(_atom_chars(key)));
    }
    if (Prolog._is_compound(key))
    {
        var term = key;
        var functor = _term_functor(key);
        var glueArgs = [];
        var arity = _term_functor_arity(key);
        state = _get_blob("state", state);
        for (var i = 0; i < arity; i++)
            glueArgs[i] = _term_arg(key, i);
        var result = state.get(functor); // Really/
        if (isNull(result))
            return _unify(value, result);
        if (Prolog._is_compound(result))
        {
            if (_term_functor(result) == Constants.thisFunctor)
            {
                if (Prolog._is_compound(_term_arg(key, 1)) && _term_functor(_term_arg(key, 1)) == Constants.colonFunctor)
                {
                    var module = _term_arg(_term_arg(key, 1), 0);
                    var goal = _term_arg(_term_arg(key, 1), 1);
                    var newGoal = addArgs(goal, glueArgs);
                    return this.unify(value, Prolog._make_compound(_term_functor(result), [_term_arg(term, 0), Prolog._make_compound(Constants.crossModuleCallFunctor, [module, newGoal])]));
                }
                else
                {
                    // No module
                    var newGoal = addArgs(_term_arg(term, 1), glueArgs);
                    return this.unify(value, Prolog._make_compound(_term_functor(result), [_term_arg(term, 0), newGoal]));
                }
            }
            Errors.typeError(Constants.gluableAtom, term);
        }
    }
    Errors.typeError(Constants.prologStateKeyAtom, state);
}


module.exports["state_to_term"] = function(state, term)
{
    throw new Error("FIXME: state_to_term not implemented");
}

module.exports["on_server"] = function(goal)
{
    // This is quite complicated because we must mix all kinds of paradigms together :(
    throw new Error("Not migrated yet");
    // Later we must yield execution. Prepare the resume code
    var resume = this.yield_control();
    var ws;
    if (this.foreign)
    {
        // We are backtracking. Try to get another solution by sending a ; and then yielding
        ws = this.foreign;
        ws.send(";");
        return "yield";
    }
    // First, create the websocket
    ws = new WebSocket(this.engine.goalURI);
    ws.onopen = function()
    {
        ws.send(Prolog.TermWriter.formatTerm({quoted:true}, 1200, goal) + ".\n");
        ws.send(";");
        // This is all we do for now. Either we will get an error, find out that the goal failed, or that it succeeded
    }
    ws.onmessage = function(event)
    {
        //console.log("Got a message: " + util.inspect(event.data));
        var term = Prolog.Parser.stringToTerm(event.data);
        if (term == Prolog.Constants.failAtom)
        {
            ws.close();
            resume(false);
        }
        else if (term == Constants.abortedAtom)
        {
            ws.close();
            resume(false);
        }
        else if (TAGOF(term) == CompoundTag)
        {
            if (_term_functor(term) == Constants.exceptionFunctor)
            {
                ws.close();
                resume(_term_arg(term, 0));
            }
            else if (_term_functor(term) == Constants.cutFunctor)
            {
                ws.close();
                resume(this.unify(goal, _term_arg(term, 0)));
            }
            else
            {
                // OK, we need a backtrack point here so we can retry
                this.create_choicepoint(ws, function() { ws.close(); });
                resume(this.unify(goal, _term_arg(term, 0)));
            }
        }
    }.bind(this);
    ws.onerror = function(event)
    {
        console.log("WS error: " + event);
        ws.close();
        try
        {
            Errors.systemError(Prolog.AtomTerm.get(event.toString()));
        }
        catch(error)
        {
            resume(error);
        }
    }
    return "yield";
}

module.exports["raise_event"] = function(a, b)
{
    // FLUX
    throw new Error("FIXME: raise_event not implemented");
}

module.exports["wait_for"] = function(fluxion)
{
    // FLUX
    throw new Error("FIXME: wait_for not implemented");
}

module.exports["get_store_state"] = function(fluxion, state)
{
    // FLUX
    throw new Error("FIXME: get_store_state not implemented");
}


module.exports["get_this"] = function(t)
{
    // This is very unlikely to work either
    return _unify(t, this.proactive_context[this.proactive_context.length-1]);
}


module.exports["bubble_event"] = function(handler, event)
{
    if (Prolog._is_compound(handler) == CompoundTag && _term_functor(handler) == Constants.thisFunctor)
    {
        var target = atom_chars(_term_arg(handler, 0));
        var resume = this.yield_control();
        target.triggerEvent(_term_arg(handler, 1), event, resume);
        return YIELD;
    }
    // Otherwise it is just a goal - go ahead and call it with one extra arg
    var goal;
    if (Prolog._is_atom(handler))
        goal = Prolog._make_compound(handler, [event]);
    else if (Prolog._is_compound(handler))
    {
        var args = [];
        var functor = _term_functor(handler);
        var arity = _term_functor_arity(handler);
        for (var i = 0; i < arity; i++)
            args[i] = _term_arg(handler, i);
        goal = Prolog._make_compound(functor, args.concat([event]));
    }
    else
        Errors.typeError(Constants.callableAtom, goal);
    var savedState = this.saveState();
    var resume = this.yield_control();
    //var resumeAlways = function(){this.restoreState(savedState); resume(true);};
    _execute(goal, function(t) { this.restoreState(savedState); resume(t)}.bind(this));
    return YIELD;
}

/* And now the DOM glue */
module.exports["remove_child"] = function(parent, child)
{
    var p = _get_blob("dom_node", parent);
    var c = _get_blob("dom_node", child);
    var found = false;
    for (var i = 0; i < p.children.length; i++)
    {
        if (p.children[i] == c)
        {
            p.children.splice(i, 1);
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to remove non-existent child");
    p.removeChild(c);
    return SUCCESS;
}

module.exports["append_child"] = function(parent, child)
{
    var p = _get_blob("dom_node", parent);
    var c = _get_blob("dom_node", child);
    p.children.push(c);
    p.appendChild(c);
    return SUCCESS;
}

module.exports["insert_before"] = function(parent, child, sibling)
{
    var p = _get_blob("dom_node", parent);
    var c = _get_blob("dom_node", child);
    var s = _get_blob("dom_node", sibling);
    var found = false;
    for (var i = 0; i < p.children.length; i++)
    {
        if (p.children[i] == s)
        {
            p.children.splice(i, 0, c);
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to insert before non-existent sibling");
    p.insertBefore(c, s);
    return SUCCESS;
}

module.exports["replace_child"] = function(parent, newChild, oldChild)
{
    var p = _get_blob("dom_node", parent);
    var n = _get_blob("dom_node", newChild);
    var o = _get_blob("dom_node", oldChild);
    var found = false;
    for (var i = 0; i < p.children.length; i++)
    {
        if (p.children[i] == o)
        {
            p.children[i] = n;
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to replace non-existent child");
    p.replaceChild(n, o);
    return SUCCESS;
}

module.exports["child_nodes"] = function(parent, children)
{
    var childNodes = _get_blob("dom_node", parent).getChildren();
    var result = Constants.emptyListAtom;
    var i = childNodes.length;
    while(i--)
        result = Prolog._make_compound(Constants.listFunctor, [_get_blob("react_component", childNodes[i]), result]);
    var v = _unify(result, children);
    if (!v)
        console.log("Failed to unify children");
    return v;
}

module.exports["create_element"] = function(context, tagname, domnode)
{
    var node = ProactiveComponentFactory.createElement(_atom_chars(tagname), _get_blob("react_component", context));
    node.setOwnerDocument(_get_blob("react_component", context));
    return _unify(domnode, Prolog._make_blob("dom_node", node));
}

module.exports["create_text_node"] = function(context, text, domnode)
{
    var node = ProactiveComponentFactory.createElement('Broken', _get_blob("react_component", context));
    node.setOwnerDocument(_get_blob("react_component", context));
    return _unify(domnode, Prolog._make_blob("dom_node", node));
}

module.exports["parent_node"] = function(node, parent)
{
    return this.unify(parent, Prolog._make_blob("dom_node", _get_blob("dom_node", node).getParent()));
}

module.exports["node_type"] = function(node, type)
{
    return _unify(type, Constants.nodeAtom);
}

module.exports["set_vdom_properties"] = function(domNode, list)
{
    if (list == Constants.emptyListAtom)
        return SUCCESS;
    var l = list;
    var properties = {};
    while (Prolog._is_compound(l) && _term_functor(l) == Constants.listFunctor)
    {
        var head = _term_arg(l, 0);
        l = _term_arg(l, 1);
        if (Prolog._is_compound(head) && _term_functor(head) == Constants.equalsFunctor)
        {
            var name = _term_arg(head, 0);
            var value = _term_arg(head, 1);
            if (!Prolog._is_atom(name))
                Errors.typeError(Constants.atomAtom, name);
            properties[_atom_chars(name)] = value;
        }
        else
            return Errors.typeError(Constants.attributeAtom, head);
    }
    if (l != Constants.emptyListAtom)
        return Errors.typeError(Constants.listAtom, list);
    _get_blob("dom_node", domNode).setProperties(properties);
    return SUCCESS;
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
    if (!Prolog._is_blob(context, "react_context"))
        return Errors.typeError(Constants.blobAtom, context);
    var parentContext = _get_blob("react_context", context);
    var resume = this.yield_control();
    var widget = new ReactWidget(parentContext,
                                 parentContext.getEngine(),
                                 _get_blob("react_context", _term_arg(properties, 0)),
                                 PrologState.fromList(_term_arg(properties, 1)),
                                 function(widget)
                                 {
                                     resume(_unify(domNode, Prolog._make_blob("react_component", widget)));
                                 }.bind(this));
    return YIELD;
}

module.exports["update_widget"] = function(newVDom, oldVDom, widget, newDomNode)
{
    var newProperties = PrologState.fromList(_term_arg(newVDom, 1));
    newProperties.map.children = _term_arg(newVDom, 2);
    var resume = this.yield_control();
    _get_blob("react_context", widget).updateWidget(newProperties, function(newWidget)
                                                    {
                                                        if (newWidget === _get_blob("react_context", widget))
                                                        {
                                                            resume(_unify(newDomNode, widget));
                                                        }
                                                        else
                                                        {
                                                            resume(_unify(newDomNode, Prolog._make_blob("react_component", newWidget)))
                                                        };
                                                    }.bind(this));

    return YIELD;
}

