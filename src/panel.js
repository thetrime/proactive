var ReactComponent = require('./react_component');

function Panel()
{
    ReactComponent.call(this);
    this.baseClassName = "proactive_container";
    this.setDOMNode(document.createElement("div"));
}

Panel.prototype = new ReactComponent;


module.exports = Panel;
