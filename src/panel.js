var ReactComponent = require('./react_component');
var Prolog = require('proscript');

// Panel is rendered as either:
// <div>
//   <div class="fieldset_legend>
//     <div class="pre_fieldset"/>
//     <div class="fieldset_text"/>
//     <div class="post_fieldset"/>
//   </div>
//   <div {contentElement}.../>
// </div>

// or

// <div {contentElement}.../>

// The domNode is always the root element.

function Panel()
{
    ReactComponent.call(this);
    this.baseClassName = "proactive_container";
    this.contentElement = document.createElement("div");
    this.setDOMNode(this.contentElement);
    this.delete_fieldset = new RegExp('(\\s|^)fieldset(\\s|$)');
    this.delete_layout = new RegExp('(\\s|^)(vertical|horizontal)_layout(\\s|$)');
    this.legendElement = null;
    this.fieldSetTextElement = null;
    this.crush = "notsure";
    this.hasScrollpaneChildren = false;
    this.hasLegend = false;
}

Panel.prototype = new ReactComponent;


Panel.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined && !ReactComponent.isNull(t.label))
    {
        if (this.legendElement == null)
        {
            this.legendElement = document.createElement("div");
            this.legendElement.className = "proactive_container horizontal_layout horizontal_fill fieldset_legend";
            var pre = document.createElement("div"); // this is the --- at the top-left
            pre.className = "pre_fieldset";
            this.legendElement.appendChild(pre);
            this.fieldSetTextElement = document.createElement("div"); // this is the text itself
            this.fieldSetTextElement.className = "fieldset_text";
            this.fieldSetTextElement.textContent = Prolog._portray(t.label);
            this.legendElement.appendChild(this.fieldSetTextElement);
            var post = document.createElement("div"); // this is the --- at the top-right
            post.className = "post_fieldset horizontal_fill";
            this.legendElement.appendChild(post);
            this.hasLegend = true;
            var f = "no_fill";
            if (this.fill == "horizontal")
                f = "horizontal_fill";
            else if (this.fill == "vertical")
                f = "vertical_fill";
            var newContentElement = document.createElement("div");
            newContentElement.className = this.baseClassName + " vertical_layout " + f;
            while (this.getDOMNode().firstChild != null)
                newContentElement.appendChild(this.contentElement.firstChild);
            this.getDOMNode().appendChild(this.legendElement);
            this.getDOMNode().appendChild(newContentElement);
            this.contentElement = newContentElement;
            this.configureHeight();
        }
        else
        {
            this.fieldSetTextElement.textContent = Prolog._portray(t.label);
        }
        this.restyle();
    }
    else if (t.label !== undefined && ReactComponent.isNull(t.label))
    {
        // Delete the legend if preset
        if (this.legendElement != null)
        {
            this.domNode.removeChild(this.legendElement);
            this.legendElement = null;
            while (this.contentElement.firstChild != null)
                this.getDOMNode().appendChild(this.contentElement.firstChild);
            this.contentElement = this.getDOMNode();
            this.hasLegend = false;
            this.restyle();
        }
    }
    if (t.background !== undefined)
    {
        if (ReactComponent.isNull(t.background))
            this.getDOMNode().style.background = "inherit";
        else if (Prolog._is_atom(t.background))
            this.getDOMNode().style.background = Prolog._atom_chars(t.background);
    }
    if (t.scroll !== undefined)
    {
        if (ReactComponent.isNull(t.scroll))
            this.scroll = "none";
        else
            this.scroll = Prolog._portray(t.scroll);
        this.restyle();
    }
    if (t.maxHeight !== undefined)
    {
        if (ReactComponent.isNull(t.maxHeight))
        {
            this.domNode.style["max-height"] = "inherit";
            //this.domNode.style["margin"] = "";
        }
        else
        {
            this.domNode.style["max-height"] = Prolog._atom_chars(t.maxHeight);
            this.domNode.style["margin"] = "auto";
        }
    }

}

Panel.prototype.restyle = function()
{
    ReactComponent.prototype.restyle.call(this);
    this.domNode.className = this.getStyle().replace(this.delete_layout, ' vertical_layout ');
    if (this.legendElement != null)
        this.contentElement.className = this.getStyle().replace(this.delete_fieldset, ' fieldset_main ') + ' fieldset_main';
    else
        this.contentElement.className = this.getStyle().replace(this.delete_fieldset, ' fieldset_main ');
    this.configureHeight();
    if (this.hasLegend)
        this.domNode.className += " fieldset";
    if (this.scroll == "both")
        this.contentElement.className += " scrollpane scroll";
    if (this.scroll == "horizontal")
        this.contentElement.className += " scrollpane scrollx";
    if (this.scroll == "vertical")
        this.contentElement.className += " scrollpane scrolly";

}

Panel.prototype.appendChild = function(t)
{
    this.contentElement.appendChild(t.getDOMNode());
    t.setParent(this);
    if (this.scrollChildChange())
    {
        this.restyle();
        if (this.parent != null && this.parent !== undefined)
            this.parent.notifyParentOfLayoutChange(this);
    }

}

Panel.prototype.insertBefore = function(t, s)
{
    this.contentElement.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
    if (this.scrollChildChange())
    {
        this.restyle();
        if (this.parent != null && this.parent !== undefined)
            this.parent.notifyParentOfLayoutChange(this);
    }

}

Panel.prototype.replaceChild = function(n, o)
{
    this.contentElement.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
    if (this.scrollChildChange())
    {
        this.restyle();
        if (this.parent != null && this.parent !== undefined)
            this.parent.notifyParentOfLayoutChange(this);
    }
}


Panel.prototype.removeChild = function(t)
{
    this.contentElement.removeChild(t.getDOMNode());
    t.setParent(null);
    if (this.scrollChildChange())
    {
        this.restyle();
        if (this.parent != null && this.parent !== undefined)
            this.parent.notifyParentOfLayoutChange(this);
    }
}

Panel.prototype.configureHeight = function ()
{
    // If the panel contains a scrollpane then it can be crushed to effectively zero size
    // If we use min-content, then the panel will be sized to include the scrollable object as if it
    // had no overflow - ie the scrollpane will never appear
    // Similarly, if we always set it to 0 then things like buttons can get squashed into oblivion by a large table
    this.hasScrollpaneChildren = (this.contentElement.getElementsByClassName("scrollpane").length > 0);
    if (this.layout == "horizontal")
    {
        if (!this.hasScrollpaneChildren)
            this.crush = "nocrush";
        else
            this.crush = "maycrush"
    }
    else
    {
        if (!this.hasScrollpaneChildren && this.baseClassName.substring("scrollpane") == -1)
            this.crush = "nocrush";
        else
            this.crush = "notsure";
    }
    this.domNode.className += " " + this.crush;
}

Panel.prototype.notifyParentOfLayoutChange = function(n)
{
    ReactComponent.prototype.notifyParentOfLayoutChange.call(n);
    if (this.scrollChildChange())
    {
        this.restyle();
        if (this.parent != null && this.parent !== undefined)
            this.parent.notifyParentOfLayoutChange(this);
    }
}

Panel.prototype.scrollChildChange = function()
{
    return this.hasScrollpaneChildren != (this.contentElement.getElementsByClassName("scrollpane").length > 0);
}

module.exports = Panel;
