var Constants = require('./constants');
var Prolog = require('proscript');

function ReactComponent()
{
    this.owner = null;
    this.domNode = null;
    this.id = null;
    this.fill = "none";
    this.layout = "vertical";
    this.baseClassName = "";
    this.children = [];
    this.parent = Constants.nullAtom;
    this.align_children = "start";
    this.justify_content = "start";
    this.blob = Prolog._make_blob("react_component", this);
}

ReactComponent.prototype.freeComponent = function(v)
{
    var children = this.getChildren();
    for (var i = 0; i < children.length; i++)
        children[i].freeComponent();
}


ReactComponent.prototype.setDOMNode = function(n)
{
    this.domNode = n;
    this.restyle();
}

ReactComponent.prototype.setParent = function(p)
{
    this.parent = p;
}

ReactComponent.prototype.setProperties = function(t)
{
    var restyleRequired = false;
    if (t.id !== undefined && this.getDOMNode() != null)
        this.getDOMNode().id = t.id;
    if (t.className !== undefined)
    {
        this.baseClassName = Prolog._atom_chars(t.className);
        restyleRequired = true;
    }
    if (t.fill !== undefined)
    {
        this.fill = Prolog._atom_chars(t.fill);
        restyleRequired = true;
    }
    if (t["align-children"] !== undefined)
    {
        this.align_children = Prolog._atom_chars(t["align-children"]);
        restyleRequired = true;
    }
    if (t["justify-content"] !== undefined)
    {
        this.justify_content = Prolog._atom_chars(t["justify-content"]);
        restyleRequired = true;
    }
    if (t.layout !== undefined)
    {
        this.layout = Prolog._atom_chars(t.layout);
        restyleRequired = true;
    }
    if (restyleRequired && this.getDOMNode() != null) // react_widget will restyle itself later once it has actually instantiated the DOM
        this.restyle();
}

ReactComponent.prototype.getDOMNode = function()
{
    return this.domNode;
}

ReactComponent.prototype.getStyle = function()
{
    var newClassName = this.baseClassName;
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

    if (this.align_children == "center")
        newClassName += " align_center";

    if (this.justify_content == "space-between")
        newClassName += " justify_space_between";
    return newClassName;
}

ReactComponent.prototype.restyle = function()
{
    this.getDOMNode().className = this.getStyle();
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

ReactComponent.make_local = function(t)
{
    return Prolog._make_local(t);
}

ReactComponent.free_local = function(t)
{
    Prolog._free_local(t);
}

module.exports = ReactComponent;
