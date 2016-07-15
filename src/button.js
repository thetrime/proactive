"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var ReactComponent = require('./react_component');

var nullAtom = new Prolog.AtomTerm("null");

function isNull(t)
{
    return t == null || (t instanceof Prolog.CompoundTerm && t.functor.equals(Prolog.Constants.curlyFunctor) && !(t.args[0].equals(nullAtom)));
}

function Button()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("button"));
}

function clickHandler(event)
{
    console.log("Event: " + event);
    console.log(this);
    this.getOwnerDocument().triggerEvent(this.clickHandler, Prolog.Constants.emptyListAtom);
}


Button.prototype = new ReactComponent;

Button.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = t.label;
    if (t.onClick !== undefined)
        this.setClickHandler(t.onClick);
}

Button.prototype.setClickHandler = function(value)
{
    if (isNull(value))
    {
        if (this.domNode.onClick !== undefined)
            this.domNode.onClick = undefined;
        return;
    }
    this.clickHandler = value;
    console.log("Creating event handler on " + this.domNode.textContent);
    this.domNode.onclick = clickHandler.bind(this);
}

module.exports = Button;
