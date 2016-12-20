var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function Panel()
{
    ReactComponent.call(this);
    this.baseClassName = "proactive_container";
    this.contentElement = document.createElement("div");
    this.setDOMNode(this.contentElement);
    this.deletion = new RegExp('(\\s|^)fieldset(\\s|$)');
}



Panel.prototype = new ReactComponent;


Panel.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        this.legendElement = document.createElement("div");
        this.legendElement.className = "proactive_container horizontal_fill horizontal_layout fieldset_legend";
        var pre = document.createElement("div"); // this is the --- at the top-left
        pre.className = "pre_fieldset";
        this.legendElement.appendChild(pre);
        var fieldSetText = document.createElement("div"); // this is the text itself
        fieldSetText.className = "fieldset_text";
        fieldSetText.textContent = Prolog._portray(t.label);
        this.legendElement.appendChild(fieldSetText);
        var post = document.createElement("div"); // this is the --- at the top-right
        post.className = "post_fieldset horizontal_fill";
        this.legendElement.appendChild(post);
        this.baseClassName = "proactive_container fieldset";
        this.domNode.insertBefore(this.legendElement, this.domNode.firstChild);
        this.contentElement = document.createElement("div");
        this.contentElement.className = "proactive_container fieldset_main";
        var f = "no_fill";
        if (this.fill == "horizontal")
            f = "horizontal_fill";
        else if (this.fill == "vertical")
            f = "vertical_fill";
        this.getDOMNode().className = "proactive_container fieldset vertical_layout " + f;
        this.getDOMNode().appendChild(this.contentElement);
        this.restyle();
    }
    if (t.scroll !== undefined)
    {
        if (ReactComponent.isNull(t.scroll))
            this.baseClassName = "proactive_container";
        else if (Prolog._portray(t.scroll) == "both")
            this.baseClassName = "proactive_container scrollpane scroll";
        else if (Prolog._portray(t.scroll) == "horizontal")
            this.baseClassName = "proactive_container scrollpane scrollx";
        else if (Prolog._portray(t.scroll) == "vertical")
            this.baseClassName = "proactive_container scrollpane scrolly";
        this.restyle();
    }
}

Panel.prototype.restyle = function()
{
    this.contentElement.className = this.getStyle().replace(this.deletion, ' fieldset_main ');
}

Panel.prototype.appendChild = function(t)
{
    this.contentElement.appendChild(t.getDOMNode());
    t.setParent(this);
}

Panel.prototype.insertBefore = function(t, s)
{
    this.contentElement.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

Panel.prototype.replaceChild = function(n, o)
{
    this.contentElement.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}


Panel.prototype.removeChild = function(t)
{
    this.contentElement.removeChild(t.getDOMNode());
    t.setParent(null);
}


module.exports = Panel;
