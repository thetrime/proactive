"use strict";

var Prolog = require('../lib/proscript2/src/core.js');
var PrologState = require('./prolog_state');
var ReactWidget = require('./react_widget');
var ProactiveComponentFactory = require('./proactive_component_factory');
var ProactiveConstants = require('./proactive_constants');
var util = require('util');

var abortedAtom = Prolog.AtomTerm.get("$aborted");

function crossModuleCall(module, goal)
{
    return Prolog.CompoundTerm.create(Prolog.Constants.crossModuleCallFunctor, [Prolog.AtomTerm.get(module), goal]);
}

function isNull(t)
{
    return (TAGOF(t) == CompoundTag && FUNCTOROF(t) == Prolog.Constants.curlyFunctor && ARGOF(t, 0) == ProactiveConstants.nullAtom);
}

function addArgs(goal, glueArgs)
{
    if (TAGOF(goal) == ConstantTag && Prolog.CTable.get(goal) instanceof Prolog.AtomTerm)
        return Prolog.CompoundTerm.create(goal, glueArgs);
    else if (TAGOF(goal) == CompoundTag)
    {
        var args = [];
        var functor = Prolog.CTable.get(FUNCTOROF(goal));
        for (var i = 0; i < functor.arity; i++)
            args[i] = ARGOF(goal,i);
        return Prolog.CompoundTerm.create(functor.name, args.concat(glueArgs));
    }
    Prolog.Errors.typeError(Prolog.Constants.callableAtom, goal);
}

/* First, Prolog-type stuff */
module.exports["."] = function(state, key, value)
{
    if (isNull(state))
        return this.unify(value, PrologState.nullTerm);
    if (!(TAGOF(state) == ConstantTag && Prolog.CTable.get(state) instanceof PrologState))
        Prolog.Errors.typeError(PrologState.prologStateAtom, state);
    if (TAGOF(key) == ConstantTag && Prolog.CTable.get(key) instanceof Prolog.AtomTerm)
    {
        return this.unify(value, Prolog.CTable.get(state).get(Prolog.CTable.get(key)));
    }
    if (TAGOF(key) == CompoundTag)
    {
        var term = key;
        var functor = Prolog.CTable.get(FUNCTOROF(key));
        var glueArgs = [];
        state = Prolog.CTable.get(state);
        for (var i = 0; i < functor.arity; i++)
            glueArgs[i] = ARGOF(key, i);
        var result = state.get(FUNCTOROF(key));
        if (isNull(result))
            return this.unify(value, result);
        if (TAGOF(result) == CompoundTag)
        {
            if (FUNCTOROF(result) == ProactiveConstants.thisFunctor)
            {
                if (TAGOF(ARGOF(key, 1)) == CompoundTag && FUNCTOROF(ARGOF(key, 1)) == ProactiveConstants.colonFunctor)
                {
                    var module = ARGOF(ARGOF(key, 1), 0);
                    var goal = ARGOF(ARGOF(key, 1), 1);
                    var newGoal = addArgs(goal, glueArgs);
                    return this.unify(value, Prolog.CompoundTerm.create(FUNCTOROF(result), [ARGOF(term, 0), Prolog.CompoundTerm.create(Prolog.Constants.crossModuleCallFunctor, [module, newGoal])]));
                }
                else
                {
                    // No module
                    var newGoal = addArgs(ARGOF(term, 1), glueArgs);
                    return this.unify(value, Prolog.CompoundTerm.create(FUNCTOROF(result), [ARGOF(term, 0), newGoal]));
                }
            }
            Prolog.Errors.typeError(ProactiveConstants.gluableAtom, term);
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
    // This is quite complicated because we must mix all kinds of paradigms together :(

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
        else if (term == abortedAtom)
        {
            ws.close();
            resume(false);
        }
        else if (TAGOF(term) == CompoundTag)
        {
            if (FUNCTOROF(term) == ProactiveConstants.exceptionFunctor)
            {
                ws.close();
                resume(ARGOF(term, 0));
            }
            else if (FUNCTOROF(term) == ProactiveConstants.cutFunctor)
            {
                ws.close();
                resume(this.unify(goal, ARGOF(term, 0)));
            }
            else
            {
                // OK, we need a backtrack point here so we can retry
                this.create_choicepoint(ws, function() { ws.close(); });
                resume(this.unify(goal, ARGOF(term, 0)));
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
    return this.unify(t, this.proactive_context[this.proactive_context.length-1]);
}


module.exports["bubble_event"] = function(handler, event)
{
    if (TAGOF(handler) == CompoundTag && FUNCTOROF(handler) == ProactiveConstants.thisFunctor)
    {
        var target = Prolog.CTable.get(ARGOF(handler, 0)).value;
        var resume = this.yield_control();
        target.triggerEvent(ARGOF(handler, 1), event, resume);
        return "yield";
    }
    // Otherwise it is just a goal - go ahead and call it with one extra arg
    var goal;
    if (TAGOF(handler) == ConstantTag && Prolog.CTable.get(handler) instanceof Prolog.AtomTerm)
        goal = Prolog.CompoundTerm.create(handler, [event]);
    else if (TAGOF(handler) == CompoundTag)
    {
        var args = [];
        var functor = Prolog.CTable.get(FUNCTOROF(handler));
        for (var i = 0; i < functor.arity; i++)
            args[i] = ARGOF(handler, i);
        goal = Prolog.CompoundTerm.create(FUNCTOROF(handler), args.concat([event]));
    }
    else
        Prolog.Errors.typeError(Prolog.Constants.callableAtom, goal);
    var savedState = this.saveState();
    var resume = this.yield_control();
    //var resumeAlways = function(){this.restoreState(savedState); resume(true);};
    this.execute(goal,
                 function() {this.restoreState(savedState); resume(true);}.bind(this),
                 function() {this.restoreState(savedState); resume(false);}.bind(this),
                 function(e) {this.restoreState(savedState); resume(e);}.bind(this));
                 //resumeAlways, resumeAlways, resumeAlways);
    return "yield";
}

/* And now the DOM glue */
module.exports["remove_child"] = function(parent, child)
{
    var p = Prolog.CTable.get(DEREF(parent));
    var c = Prolog.CTable.get(DEREF(child));
    var found = false;
    for (var i = 0; i < p.value.children.length; i++)
    {
        if (p.value.children[i] == c.value)
        {
            p.value.children.splice(i, 1);
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to remove non-existent child");
    p.value.removeChild(c.value);
    return true;
}

module.exports["append_child"] = function(parent, child)
{
    var p = Prolog.CTable.get(DEREF(parent));
    var c = Prolog.CTable.get(DEREF(child));
    p.value.children.push(c.value);
    p.value.appendChild(c.value);
    return true;
}

module.exports["insert_before"] = function(parent, child, sibling)
{
    var p = Prolog.CTable.get(DEREF(parent));
    var c = Prolog.CTable.get(DEREF(child));
    var s = Prolog.CTable.get(DEREF(sibling));
    var found = false;
    for (var i = 0; i < p.value.children.length; i++)
    {
        if (p.value.children[i] == s.value)
        {
            p.value.children.splice(i, 0, c.value);
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to insert before non-existent sibling");
    p.value.insertBefore(c.value, s.value);
    return true;
}

module.exports["replace_child"] = function(parent, newChild, oldChild)
{
    var p = Prolog.CTable.get(DEREF(parent));
    var n = Prolog.CTable.get(DEREF(newChild));
    var o = Prolog.CTable.get(DEREF(oldChild));
    var found = false;
    for (var i = 0; i < p.value.children.length; i++)
    {
        if (p.value.children[i] == o.value)
        {
            p.value.children[i] = n.value;
            found = true;
            break;
        }
    }
    if (!found)
        throw new Error("Attempt to replace non-existent child");
    p.value.replaceChild(n.value, o.value);
    return true;}

module.exports["child_nodes"] = function(parent, children)
{
    var childNodes = Prolog.CTable.get(DEREF(parent)).value.getChildren();
    var result = Prolog.Constants.emptyListAtom;
    var i = childNodes.length;
    while(i--)
        result = Prolog.CompoundTerm.create(Prolog.Constants.listFunctor, [Prolog.BlobTerm.get("react_component", childNodes[i]), result]);
    var v = this.unify(result, children);
    if (!v)
        console.log("Failed to unify children");
    return v;
}

module.exports["create_element"] = function(context, tagname, domnode)
{
    var node = ProactiveComponentFactory.createElement(Prolog.CTable.get(tagname).value, Prolog.CTable.get(context).value);
    node.setOwnerDocument(Prolog.CTable.get(context).value);
    return this.unify(domnode, Prolog.BlobTerm.get("dom_node", node));
}

module.exports["create_text_node"] = function(context, text, domnode)
{
    var node = ProactiveComponentFactory.createElement('Broken', Prolog.CTable.get(context).value);
    node.setOwnerDocument(Prolog.CTable.get(context).value);
    return this.unify(domnode, Prolog.BlobTerm.get("dom_node", node));
    //throw new Error("create_text_node/3 is not implemented and probably never will be. Do not create text nodes!");
}

module.exports["parent_node"] = function(node, parent)
{
    return this.unify(parent, Prolog.BlobTerm.get("react_component", Prolog.CTable.get(node).value.getParent()));
}

module.exports["node_type"] = function(node, type)
{
    return this.unify(type, ProactiveConstants.nodeAtom);
}

module.exports["set_vdom_properties"] = function(domNode, list)
{
    if (list == Prolog.Constants.emptyListAtom)
        return true;
    var l = list;
    var properties = {};
    while (TAGOF(l) == CompoundTag && FUNCTOROF(l) == Prolog.Constants.listFunctor)
    {
        var head = ARGOF(l, 0);
        l = ARGOF(l, 1);
        if (TAGOF(head) == CompoundTag && FUNCTOROF(head) == ProactiveConstants.equalsFunctor)
        {
            var name = ARGOF(head, 0);
            var value = ARGOF(head, 1);
            Prolog.Utils.must_be_atom(name);
            properties[Prolog.CTable.get(name).value] = value;
        }
        else
            Prolog.Errors.typeError(ProactiveConstants.attributeAtom, head);
    }
    if (l != Prolog.Constants.emptyListAtom)
        Prolog.Errors.typeError(Prolog.Constants.listAtom, list);
    Prolog.CTable.get(domNode).value.setProperties(properties);
    return true;
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
    var parentContext = Prolog.CTable.get(context).value;
    var resume = this.yield_control();
    var widget = new ReactWidget(parentContext,
                                 parentContext.getEngine(),
                                 Prolog.CTable.get(ARGOF(properties, 0)).value,
                                 PrologState.fromList(ARGOF(properties, 1)),
                                 function(widget)
                                 {
                                     resume(this.unify(domNode, Prolog.BlobTerm.get("react_component", widget)));
                                 }.bind(this));
    return "yield";
}

module.exports["update_widget"] = function(newVDom, oldVDom, widget, newDomNode)
{
    var newProperties = PrologState.fromList(ARGOF(newVDom, 1));
    newProperties.map.children = ARGOF(newVDom, 2);
    var resume = this.yield_control();
    Prolog.CTable.get(widget).value.updateWidget(newProperties, function(newWidget)
                              {
                                  if (newWidget === Prolog.CTable.get(widget).value)
                                  {
                                      resume(this.unify(newDomNode, widget));
                                  }
                                  else
                                  {
                                      resume(this.unify(newDomNode, Prolog.BlobTerm.get("react_component", newWidget)))
                                  };
                              }.bind(this));

    return "yield";
}

