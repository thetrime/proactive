var ReactComponent = require('./react_component');

var default_weight = 1;

function TableHeader()
{
    ReactComponent.call(this);
    this.weights = [];
    this.tr = document.createElement("tr");
    this.setDOMNode(this.tr);
    this.sum = 0;
    this.elements = [];
}

TableHeader.prototype = new ReactComponent;


// This is tricky since t.weight may be undefined, a number or an atom like "40%"
// When adjusting this.sum, we must treat 40% as default_weight, but when updating weights[] we must leave it as 40%
// That is why it looks like we calculate the same value a lot: new_value is NOT the same as n.value || default_value!
TableHeader.prototype.appendChild = function(t)
{
    var cell = document.createElement("th");
    cell.className = "react_table_header_cell";
    cell.appendChild(t.domNode);
    this.elements.push(cell);
    this.getDOMNode().appendChild(cell);
    this.weights[this.children.indexOf(t)] = t.weight || default_weight;
    this.sum += (Number(t.weight) || default_weight);
    t.setParent(this);
    // FIXME: Notify parent that it may need to relayout
}

TableHeader.prototype.getElements = function()
{
    return this.elements;
}
// FIXME: Implement other methods

module.exports = TableHeader;
