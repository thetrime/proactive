function ReactComponent()
{
    this.owner = null;
    this.domNode = null;
    this.id = null;
    this.fill = "none";
    this.layout = "vertical";
    this.baseClassName = "";
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
        this.baseClassName = t.className;
        restyleRequired = true;
    }
    if (t.fill !== undefined)
    {
        this.fill = t.fill;
        restyleRequired = true;
    }
    if (t.layout !== undefined)
    {
        this.layout = t.layout;
        restyleRequired = true;
    }
    if (restyleRequired)
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
        newClassName += " horizontal_layout";
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


ReactComponent.prototype.appendChild = function(t)
{
    this.domNode.appendChild(t.getDOMNode());
}

module.exports = ReactComponent;
