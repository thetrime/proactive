var ReactComponent = require('./react_component');

function Tree()
{
    ReactComponent.call(this);
    this.baseClassName = "tree subtree"
    this.setDOMNode(document.createElement("ol"));
}
Tree.prototype = new ReactComponent;

module.exports = Tree;
