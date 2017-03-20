var ReactComponent = require('./react_component');

var default_weight = 1;

function TableHeader()
{
    ReactComponent.call(this);
    this.weights = [];
    var node = document.createElement("tr");
    node.className = "react_table_header_row";
    this.setDOMNode(node);
    this.sum = 0;
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
    this.domNode.appendChild(cell);
    this.weights[this.children.indexOf(t)] = t.weight || default_weight;
    this.sum += (Number(t.weight) || default_weight);
    t.setParent(this);
    this.recomputeRatios();
}

TableHeader.prototype.removeChild = function(c)
{
    this.sum -= (c.weight || default_weight);
    this.weights.splice(this.children.indexOf(c), 1);
    this.recomputeRatios();
    c.setParent(null);
    this.getDOMNode().removeChild(this.getDOMNode().childNodes[this.children.indexOf(c)]);
}

TableHeader.prototype.replaceChild = function(n, o)
{
    var old_weight = Number(o.weight) || default_weight;
    var new_weight = Number(n.weight) || default_weight;
    this.sum = this.sum - old_weight + new_weight;
    this.weights[this.children.indexOf(o)] = (n.weight || default_weight);
    this.recomputeRatios();
    o.setParent(null);
    n.setParent(this);
    this.getDOMNode().replaceChild(this.getDOMNode().childNodes[this.children.indexOf(n), o.getDOMNode()]);
}

TableHeader.prototype.insertBefore = function(t, s)
{
    var new_weight = Number(t.weight) || default_weight;
    this.weights.splice(children.indexOf(s), 0, (t.weight || default_weight));
    this.sum += new_weight;
    this.recomputeRatios();
    t.setParent(this);
    var cell = document.createElement("th");
    cell.className = "react_table_header_cell";
    cell.appendChild(t.domNode);
    getDOMNode().insertBefore(cell, this.getDOMNode().childNodes[this.children.indexOf(s)]);
}

TableHeader.prototype.recomputeRatios = function()
{
    var th = this.domNode.childNodes;
    for (i = 0; i < this.children.length; i++)
    {
        if (this.weights[i] == default_weight)
            th[i].style['width'] = "";
        else if (typeof this.weights[i] == "number")
            th[i].style['width'] = (100 * (this.weights[i] / this.sum)) + "%";
        else if (typeof this.weights[i] == "string")
            th[i].style['width'] = this.weights[i];
    }
}

TableHeader.prototype.notifyParentOfLayoutChange = function(n)
{
    var old_weight = Number(this.weights[this.children.indexOf(n)]) || default_weight;
    var new_weight = Number(n.weight) || default_weight;
    this.weights[this.children.indexOf(n)] = n.weight || default_weight;
    this.sum = this.sum - old_weight + new_weight;
    this.recomputeRatios();
}

module.exports = TableHeader;
