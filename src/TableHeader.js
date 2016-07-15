var ReactComponent = require('./react_component');

function Table()
{
    this.setDOMNode(document.createElement("table"));
}

Table.prototype = new ReactComponent;

module.exports = Table;
