var ReactComponent = require('./react_component');

function PopupMenu(name)
{
    ReactComponent.call(this);
    var node = document.createElement("div");
    this.baseClassName = "proactive_menu";
    this.setDOMNode(node);
}

PopupMenu.prototype = new ReactComponent;


module.exports = PopupMenu;
