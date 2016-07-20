"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var PrologState = require('./prolog_state');
var ReactWidget = require('./react_widget');
var ProactiveComponentFactory = require('./proactive_component_factory');
var ProactiveConstants = require('./proactive_constants');
var util = require('util');

function crossModuleCall(module, goal)
{
    return new Prolog.CompoundTerm(Prolog.Constants.crossModuleCallFunctor, [new Prolog.AtomTerm(module), goal]);
}

function isNull(t)
{
    return (t instanceof Prolog.CompoundTerm && t.functor.equals(Prolog.Constants.curlyFunctor) && t.args[0].equals(ProactiveConstants.nullAtom));
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
            if (result.functor.equals(ProactiveConstants.thisFunctor))
            {
                if (key.args[1] instanceof CompoundTerm && key.args[1].functor.equals(ProactiveConstants.colonFunctor))
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
        if (term.equals(Prolog.Constants.failAtom))
        {
            ws.close();
            resume(false);
        }
        else if (term instanceof Prolog.AtomTerm && term.value == "$aborted")
        {
            ws.close();
            resume(false);
        }
        else if (term instanceof Prolog.CompoundTerm)
        {
            if (term.functor.equals(ProactiveConstants.exceptionFunctor))
            {
                ws.close();
                resume(term.args[0]);
            }
            else if (term.functor.equals(ProactiveConstants.cutFunctor))
            {
                ws.close();
                resume(this.unify(goal, term.args[0]));
            }
            else
            {
                // OK, we need a backtrack point here so we can retry
                this.create_choicepoint(ws, function() { ws.close(); });
                resume(this.unify(goal, term.args[0]));
            }
        }
    }.bind(this);
    ws.onerror = function(event)
    {
        console.log("WS error: " + event);
        ws.close();
        try
        {
            Errors.systemError(new Prolog.AtomTerm(event.toString()));
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
    return this.unify(t, new Prolog.BlobTerm("react_context", this.proactive_context[this.proactive_context.length-1]));
}


module.exports["bubble_event"] = function(handler, event)
{
    if (handler instanceof Prolog.CompoundTerm && handler.functor.equals(ProactiveConstants.thisFunctor))
    {
        var target = handler.args[0].value;
        var resume = this.yield_control();
        target.triggerEvent(handler.args[1], event, resume);
        return "yield";
    }
    // Otherwise it is just a goal - go ahead and call it with one extra arg
    var goal;
    if (handler instanceof Prolog.AtomTerm)
        goal = new Prolog.CompoundTerm(handler, [event]);
    else if (handler instanceof Prolog.CompoundTerm)
        goal = new Prolog.CompoundTerm(handler.functor, handler.args.concat([event]));
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
    var p = parent.dereference();
    var c = child.dereference();
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
    var p = parent.dereference();
    var c = child.dereference();
    p.value.children.push(c.value);
    p.value.appendChild(c.value);
    return true;
}

module.exports["insert_before"] = function(parent, child, sibling)
{
    var p = parent.dereference();
    var c = child.dereference();
    var s = sibling.dereference();
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
    var p = parent.dereference();
    var n = newChild.dereference();
    var o = oldChild.dereference();
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
    var childNodes = parent.dereference().value.getChildren();
    var result = Prolog.Constants.emptyListAtom;
    var i = childNodes.length;
    while(i--)
        result = new Prolog.CompoundTerm(Prolog.Constants.listFunctor, [new Prolog.BlobTerm("react_component", childNodes[i]), result]);
    var v = this.unify(result, children);
    if (!v)
        console.log("Failed to unify children");
    return v;
}

module.exports["create_element"] = function(context, tagname, domnode)
{
    var node = ProactiveComponentFactory.createElement(tagname.value, context.value);
    node.setOwnerDocument(context.value);
    return this.unify(domnode, new Prolog.BlobTerm("dom_node", node));
}

module.exports["create_text_node"] = function(context, text, domnode)
{
    throw new Error("create_text_node/3 is not implemented and probably never will be. Do not create text nodes!");
}

module.exports["parent_node"] = function(node, parent)
{
    return this.unify(parent, new Prolog.BlobTerm("react_component", node.value.getParent()));
}

module.exports["node_type"] = function(node, type)
{
    return this.unify(type, ProactiveConstants.nodeAtom);
}

module.exports["set_vdom_properties"] = function(domNode, list)
{
    if (Prolog.Constants.emptyListAtom.equals(list))
        return true;
    var l = list;
    var properties = {};
    while (l instanceof Prolog.CompoundTerm && l.functor.equals(Prolog.Constants.listFunctor))
    {
        var head = l.args[0].dereference();
        l = l.args[1].dereference();
        if (head instanceof Prolog.CompoundTerm && head.functor.equals(ProactiveConstants.equalsFunctor))
        {
            var name = head.args[0];
            var value = head.args[1];
            Prolog.Utils.must_be_atom(name);
            properties[name.value] = value;
        }
        else
            Prolog.Errors.typeError(ProactiveConstants.attributeAtom, head);
    }
    if (!Prolog.Constants.emptyListAtom.equals(l))
        Prolog.Errors.typeError(Prolog.Constants.listAtom, list);
    domNode.value.setProperties(properties);
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
    var parentContext = context.value;
    var resume = this.yield_control();
    var widget = new ReactWidget(parentContext, parentContext.getEngine(), properties.args[0].value, PrologState.fromList(properties.args[1]),
                                 function(widget)
                                 {
                                     resume(this.unify(domNode, new Prolog.BlobTerm("react_component", widget)));
                                 }.bind(this));
    return "yield";
}

module.exports["update_widget"] = function(newVDom, oldVDom, widget, newDomNode)
{
    var newProperties = PrologState.fromList(newVDom.args[1]);
    newProperties.map.children = newVDom.args[2];
    var resume = this.yield_control();
    widget.value.updateWidget(newProperties, function(newWidget)
                              {
                                  if (newWidget === widget.value)
                                  {
                                      resume(this.unify(newDomNode, widget));
                                  }
                                  else
                                  {
                                      resume(this.unify(newDomNode, new Prolog.BlobTerm("react_component", newWidget)))
                                  };
                              }.bind(this));

    return "yield";
}

