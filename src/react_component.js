var Constants = require('./constants');
var Prolog = require('proscript');

function ReactComponent()
{
    this.owner = null;
    this.domNode = null;
    this.id = null;
    this.fill = "none";
    this.layout = "vertical";
    this.wrap = "nowrap";
    this.baseClassName = "";
    this.overrideClass = ""
    this.children = [];
    this.parent = null;
    this.align_children = "default";
    this.justify_content = "start";
    this.blob = Prolog._make_blob("react_component", this);
    this.makeContextMenu = null;
}

ReactComponent.prototype.renderContextMenu = function(event)
{
    if (this.makeContextMenu != null)
    {
        console.log(Prolog._portray(this.makeContextMenu));
        this.getOwnerDocument().renderAuxComponent(this.makeContextMenu, ReactComponent.serialize({x: Prolog._make_integer(event.clientX),
                                                                                                   y: Prolog._make_integer(event.clientY)}),
                                                   function(dom)
                                                   {
                                                       ReactComponent.dismissPopups();
                                                       document.getElementById("container").appendChild(dom.getDOMNode());
                                                       dom.getDOMNode().style.top = event.clientY;
                                                       dom.getDOMNode().style.left = event.clientX;
                                                   });
        event.preventDefault();
    }
}

ReactComponent.prototype.freeComponent = function(v)
{
    var children = this.getChildren();
    for (var i = 0; i < children.length; i++)
        children[i].freeComponent();
}

ReactComponent.dismissPopups = function()
{
    var oldmenus = document.getElementById("container").getElementsByClassName("proactive_menu");
    while(oldmenus[0])
        oldmenus[0].parentNode.removeChild(oldmenus[0]);
}



ReactComponent.prototype.setDOMNode = function(n)
{
    this.domNode = n;
    this.domNode.oncontextmenu = this.renderContextMenu.bind(this);
    this.restyle();
}

ReactComponent.prototype.notifyParentOfLayoutChange = function(n)
{
}

ReactComponent.prototype.setParent = function(p)
{
    this.parent = p;
}

ReactComponent.prototype.setProperties = function(t)
{
    var restyleRequired = false;
    var mustNotifyParent = false;
    if (t.id !== undefined && this.getDOMNode() != null)
        this.getDOMNode().id = Prolog._portray(t.id);
    if (t["class"] !== undefined)
    {
        if (ReactComponent.isNull(t["class"]))
            this.overrideClass = "";
        else
            this.overrideClass = Prolog._atom_chars(t["class"]);
        restyleRequired = true;
    }
    if (t.fill !== undefined)
    {
        if (ReactComponent.isNull(t.fill))
            this.fill = "none";
        else
            this.fill = Prolog._atom_chars(t.fill);
        restyleRequired = true;
    }
    if (t["align-children"] !== undefined)
    {
        if (ReactComponent.isNull(t["align-children"]))
            this.align_children = "default";
        else
            this.align_children = Prolog._atom_chars(t["align-children"]);
        restyleRequired = true;
    }
    if (t["justify-content"] !== undefined)
    {
        if (ReactComponent.isNull(t["justify-content"]))
            this.justify_content = "start";
        else
            this.justify_content = Prolog._atom_chars(t["justify-content"]);
        restyleRequired = true;
    }
    if (t.layout !== undefined)
    {
        if (ReactComponent.isNull(t.layout))
            this.layout = "vertical";
        else
            this.layout = Prolog._atom_chars(t.layout);
        restyleRequired = true;
    }
    if (t.wrap !== undefined)
    {
        if (ReactComponent.isNull(t.wrap))
            this.wrap = "nowrap";
        else
            this.wrap = Prolog._atom_chars(t.wrap);
        restyleRequired = true;
    }
    if (t.weight !== undefined)
    {
        if (Prolog._is_integer(t.weight))
        {
            this.weight = Prolog._numeric_value(t.weight);
        }
        else if (Prolog._is_atom(t.weight))
        {
            var w = Prolog._atom_chars(t.weight);
            if (!isNaN(Number(w)))
                this.weight = Number(w)
            else if (w.charAt(w.length-1) == "%")
                this.weight = w;
            else
                this.weight = undefined;
        }
        else
        {
            this.weight = undefined;
        }
        mustNotifyParent = true;
    }
    if (t.maxWidth !== undefined)
    {
        if (ReactComponent.isNull(t.maxWidth))
            this.maxWidth = null;
        else
            this.maxWidth = Prolog._atom_chars(t.maxWidth);
        restyleRequired = true;
    }
    if (t.renderContextMenu !== undefined)
    {
        if (this.makeContextMenu != null)
            Prolog._free_local(this.makeContextMenu)
        if (ReactComponent.isNull(t.renderContextMenu))
            this.makeContextMenu = null;
        else
            this.makeContextMenu = Prolog._make_local(t.renderContextMenu);
    }
    if (mustNotifyParent && this.parent != null)
        this.parent.notifyParentOfLayoutChange(this);
    if (restyleRequired && this.getDOMNode() != null) // react_widget will restyle itself later once it has actually instantiated the DOM
        this.restyle();
}

ReactComponent.prototype.getDOMNode = function()
{
    return this.domNode;
}

ReactComponent.prototype.getStyle = function()
{
    var newClassName = this.baseClassName + " " + this.overrideClass;
    if (this.fill == "none")
        newClassName += " no_fill";
    else if (this.fill == "horizontal")
        newClassName += " horizontal_fill";
    else if (this.fill == "vertical")
        newClassName += " vertical_fill";
    else if (this.fill == "both")
        newClassName += " vertical_fill horizontal_fill";

    if (this.layout == "vertical")
        newClassName += " vertical_layout";
    else if (this.layout == "horizontal")
        newClassName += " horizontal_layout";
    else if (this.layout == "grid") // FIXME: This is a hack for testing!
        newClassName += " horizontal_layout";

    if (this.align_children == "start")
        newClassName += " align_start";
    if (this.align_children == "end")
        newClassName += " align_end";

    if (this.justify_content == "space-between")
        newClassName += " justify_space_between";
    if (this.justify_content == "space-around")
        newClassName += " justify_space_around";
    if (this.justify_content == "center")
        newClassName += " justify_center";

    if (this.wrap == "wrap")
        newClassName += " wrap";
    else if (this.wrap == "wrap-reverse")
        newClassName += " wrap_reverse";
    return newClassName;
}

ReactComponent.prototype.restyle = function()
{
    this.getDOMNode().className = this.getStyle();
    // the DOM node may not be an actual DOM node. For example, ChartDataSet
    if (this.getDOMNode().style != null)
    {
        if (this.maxWidth == null)
        {
            this.getDOMNode().style["max-width"] = "";
            this.getDOMNode().style["margin"] = "";
        }
        else
        {
            this.getDOMNode().style["max-width"] = this.maxWidth
            if (this.align_children == "start")
                this.getDOMNode().style["margin-right"] = "";
            else if (this.align_children == "end")
                this.getDOMNode().style["margin-left"] = "auto";
            else if (this.align_children == "center")
                this.getDOMNode().style["margin"] = "auto";
            else
                this.getDOMNode().style["margin"] = "inherit";
        }
    }

}

ReactComponent.prototype.setOwnerDocument = function(d)
{
    this.owner = d;
}

ReactComponent.prototype.getOwnerDocument = function()
{
    return this.owner;
}

ReactComponent.prototype.getChildren = function()
{
    return this.children;
}

ReactComponent.prototype.appendChild = function(t)
{
    this.domNode.appendChild(t.getDOMNode());
    t.setParent(this);
}

ReactComponent.prototype.insertBefore = function(t, s)
{
    this.domNode.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

ReactComponent.prototype.replaceChild = function(n, o)
{
    this.domNode.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

ReactComponent.prototype.getParent = function()
{
    return this.parent;
}

ReactComponent.prototype.removeChild = function(t)
{
    this.domNode.removeChild(t.getDOMNode());
    t.setParent(null);
}

ReactComponent.isNull = function(t)
{
    return t == null || (Prolog._is_compound(t) && Prolog._term_functor(t) == Constants.curlyFunctor && (Prolog._term_arg(t, 0) == Constants.nullAtom));
}

ReactComponent.serialize = function(properties)
{
    var result = Constants.emptyListAtom;
    var keys = Object.keys(properties);
    for (var i = 0; i < keys.length; i++)
    {
        result = Prolog._make_compound(Constants.listFunctor, [Prolog._make_compound(Constants.equalsFunctor, [Prolog._make_atom(keys[i]), properties[keys[i]]]), result]);
    }
    //console.log("Made an event from " + keys[0] + ":" + properties[keys[0]] + "-> " + Prolog._format_term(null, 1200, result))
    return result;
}

ReactComponent.arrayToList = function(array)
{
    var result = Constants.emptyListAtom;
    for (var i = 0; i < array.length; i++)
        result = Prolog._make_compound(Constants.listFunctor, [array[i], result]);
    return result;
}

ReactComponent.booleanValue = function(t)
{
    return t == Constants.trueAtom;
}

ReactComponent.numericValueOr = function(value, otherwise)
{
    if (Prolog._is_integer(value))
    {
        return Prolog._numeric_value(value);
    }
    else if (Prolog._is_atom(value))
    {
        var w = Prolog._atom_chars(value);
        if (!isNaN(Number(w)))
            return Number(w)
        else
            return otherwise;
    }
    else
        return otherwise;
}

ReactComponent.make_local = function(t)
{
    return Prolog._make_local(t);
}

ReactComponent.free_local = function(t)
{
    Prolog._free_local(t);
}

module.exports = ReactComponent;
