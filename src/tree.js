var ReactComponent = require('./react_component');
var Prolog = require('proscript');

var id = 0;

function Tree()
{
    ReactComponent.call(this);
    this.id = "$_root" + (id++);
    this.baseClassName = "tree subtree";
    this.setDOMNode(document.createElement("ol"));
    this.root = document.createElement("li");
    this.root.className = "root";
    this.input = document.createElement("input");
    this.input.type = "checkbox";
    this.input.className = "tree_input";
    this.input.id = this.id;
    this.label = document.createElement("label");
    this.label.htmlFor = this.id;
    this.ol = document.createElement("ol");
    this.ol.className = "subtree";
    this.root.appendChild(this.input);
    this.root.appendChild(this.label);
    this.root.appendChild(this.ol);
    this.getDOMNode().appendChild(this.root);

}
Tree.prototype = new ReactComponent;

Tree.prototype.appendChild = function(t)
{
    this.ol.appendChild(t.getDOMNode());
    t.setParent(this);
    if (this.children.length == 1)
        this.configureClass();
}

Tree.prototype.insertBefore = function(t, s)
{
    this.ol.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
    if (this.children.length == 1)
        this.configureClass();

}

Tree.prototype.replaceChild = function(n, o)
{
    this.ol.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

Tree.prototype.removeChild = function(t)
{
    this.ol.removeChild(t.getDOMNode());
    t.setParent(null);
    if (this.children.length == 0)
        this.configureClass();
}

Tree.prototype.configureClass = function()
{
    this.input.className = "tree_input " + ((this.children.length == 0)?"leaf":"trunk");
}


Tree.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        this.label.textContent = Prolog._portray(t.label);
    }
}

module.exports = Tree;
