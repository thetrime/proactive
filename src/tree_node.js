var ReactComponent = require('./react_component');
var Prolog = require('proscript');

var id = 0;

function TreeNode()
{
    ReactComponent.call(this);
    this.baseClassName = "tree_node";
    var li = document.createElement("li");
    this.setDOMNode(li);
    this.ol = document.createElement("ol");
    this.ol.className = "subtree";
    this.input = document.createElement("input");
    this.input.type ="checkbox";
    this.id = "$_node" + (id++);
    this.input.id = this.id;
    this.input.className ="tree_input";
    this.label = document.createElement("label");
    this.label.htmlFor = this.id;
    li.appendChild(this.input);
    li.appendChild(this.label);
    li.appendChild(this.ol);
    console.log("XXX");

}
TreeNode.prototype = new ReactComponent;

TreeNode.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        this.label.textContent = Prolog._portray(t.label);
    }
}

TreeNode.prototype.appendChild = function(t)
{
    this.ol.appendChild(t.getDOMNode());
    t.setParent(this);
    if (this.children.length == 1)
        this.configureClass();
}

TreeNode.prototype.insertBefore = function(t, s)
{
    this.ol.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
    if (this.children.length == 1)
        this.configureClass();

}

TreeNode.prototype.replaceChild = function(n, o)
{
    this.ol.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

TreeNode.prototype.removeChild = function(t)
{
    this.ol.removeChild(t.getDOMNode());
    t.setParent(null);
    if (this.children.length == 0)
        this.configureClass();
}

TreeNode.prototype.configureClass = function()
{
    this.input.className = "tree_input " + ((this.children.length == 0)?"leaf":"trunk");
}

module.exports = TreeNode;
