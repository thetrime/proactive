var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Label()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("label"));
}
Label.prototype = new ReactComponent;

Label.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        var v = Prolog._portray(t.label);
        if (v.lastIndexOf("<html>", 0) == 0)
            this.domNode.innerHTML = v;
        else
            this.domNode.textContent = v;
        // If no title is supplied and there is no title on the node, default it to the label when the label is set
        if (t.title == undefined)
        {
            if (Prolog._is_atom(t.label))
                t.title = t.label;
            else
                t.title = Prolog._make_atom(Prolog._portray(t.label));
        }
    }
    if (t["for"] !== undefined)
    {
        var f = t["for"];
        if (ReactComponent.isNull(f))
            this.domNode.htmlFor = '';
        else
            this.domNode.htmlFor = Prolog._portray(f);
    }
    if (t.align !== undefined)
    {
        if (ReactComponent.isNull(t.align))
            this.domNode.style["text-align"] = "";
        else
            this.domNode.style["text-align"] = Prolog._atom_chars(t.align);
    }
    if (t.overflow !== undefined)
    {
        if (t.overflow == Constants.ellipsisAtom)
        {
            this.domNode.style["white-space"] = "nowrap";
            this.domNode.style["text-overflow"] = "ellipsis";
            this.domNode.style["overflow"] = "hidden";
        }
        else
        {
            this.domNode.style["white-space"] = "normal";
            this.domNode.style["text-overflow"] = "clip";
            this.domNode.style["overflow"] = "visible";
        }
    }

    if (t.title !== undefined)
    {
        if (ReactComponent.isNull(t.title) || !Prolog._is_atom(t.title))
            this.domNode.title = "";
        else
            this.domNode.title = Prolog._atom_chars(t.title);
    }


}

module.exports = Label;
