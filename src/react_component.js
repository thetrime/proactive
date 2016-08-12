var Constants = require('./constants');
var Prolog = require('../lib/proscript2/build/proscript.js');

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
    this.blob = Prolog._make_blob("react_component", this);

}

ReactComponent.prototype.setDOMNode = function(n)
{
    this.domNode = n;
    this.restyle();
}

ReactComponent.prototype.setProperties = function(t)
{
    var restyleRequired = false;
    if (t.id !== undefined)
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

ReactComponent.prototype.restyle = function()
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
    {
        newClassName += " horizontal_layout";
    }
    this.getDOMNode().className = newClassName;
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
    t.parent = this;
}

ReactComponent.prototype.insertBefore = function(t, s)
{
    this.domNode.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.parent = this;
}

ReactComponent.prototype.replaceChild = function(n, o)
{
    this.domNode.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.parent = this;
    o.parent = null;
}

ReactComponent.prototype.getParent = function()
{
    return this.parent;
}

ReactComponent.prototype.removeChild = function(t)
{
    this.domNode.removeChild(t.getDOMNode());
    t.parent = null;
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

ReactComponent.booleanValue = function(t)
{
    return t == Constants.trueAtom;
}


module.exports = ReactComponent;
