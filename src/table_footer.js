var ReactComponent = require('./react_component');

function TableFooter()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tfoot"));
}
TableFooter.prototype = new ReactComponent;

module.exports = TableFooter;
