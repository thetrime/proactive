var ReactComponent = require('./react_component');
var Prolog = require('proscript');

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
        this.domNode.style["text-align"] = Prolog._atom_chars(t.align);
    }
    if (t.title !== undefined)
    {
        if (ReactComponent.isNull(t.title))
            this.domNode.title = "";
        else
            this.domNode.title = Prolog._atom_chars(t.title);
    }


}

module.exports = Label;
