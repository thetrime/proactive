var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Grid()
{
    ReactComponent.call(this);
    this.baseClassName = "table_grid"
    this.setDOMNode(document.createElement("table"));
    this.colgroup = document.createElement("colgroup");
    this.tbody = document.createElement("tbody");
    this.getDOMNode().appendChild(this.colgroup);
    this.getDOMNode().appendChild(this.tbody);
    this.last_row = null;
    this.weights = [0];
    this.cells = [];
}
Grid.prototype = new ReactComponent;

Grid.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    var must_relayout = false;
    if (t.weights !== undefined)
    {
        var list = t.weights;
        var newWeights = [];
        while (Prolog._is_compound(list) && Prolog._term_functor(list) == Constants.listFunctor)
        {
            newWeights.push(ReactComponent.numericValueOr(Prolog._term_arg(list, 0), 0));
            list = Prolog._term_arg(list, 1);
        }
        if (list != Constants.emptyListAtom)
            console.log("Bad weights list!");
        // If the weights have changed, relayout
        if (newWeights.length != this.weights.length || !newWeights.every(function(v,i) { return v === this.weights[i]}.bind(this)))
            must_relayout = true;
        this.weights = newWeights;
    }
    /* Not supported in table grid
    if (t.gap !== undefined)
    {
        if (ReactComponent.isNull(t.gap))
            this.getDOMNode().style['grid-gap'] = "";
        else
            this.getDOMNode().style['grid-gap'] = Prolog._atom_chars(t.gap);
    }
    */
    if (must_relayout)
        this.relayout();
}

Grid.prototype.relayout = function(t)
{
    var template = "";
    var total = 0;
    while (this.colgroup.hasChildNodes())
        this.colgroup.removeChild(this.colgroup.lastChild);

    for (var i = 0; i < this.weights.length; i++)
    {
        var col = document.createElement("col");
        this.colgroup.appendChild(col);
        // This is not HTML5. But then, we are trying to cater to browsers that dont have css-grid, so...
        if (this.weights[i] != 0)
            col.width = this.weights[i] + '*';
    }
}

Grid.prototype.rearrangeChildren = function()
{
    var new_tbody = document.createElement('tbody');
    var r = null;
    for (var i = 0; i < this.cells.length; i++)
    {
        if (i % this.weights.length == 0)
        {
            r = document.createElement("tr");
            new_tbody.appendChild(r);
        }
        r.appendChild(this.cells[i]);
    }
    this.tbody.parentNode.replaceChild(new_tbody, this.tbody);
    this.tbody = new_tbody;
}

Grid.prototype.appendChild = function(t)
{
    if (this.last_row == null)
    {
        this.last_row = document.createElement("tr");
        this.tbody.appendChild(this.last_row);
    }
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    if (this.last_row.children.length == this.weights.length)
    {
        this.last_row = document.createElement("tr");
        this.tbody.appendChild(this.last_row);
    }
    this.last_row.appendChild(cell);
    t.setParent(this);
    this.cells.push(cell);
}

Grid.prototype.insertBefore = function(t, s)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    var index = this.children.indexOf(s);
    this.cells.splice(index, 0, cell);
    t.setParent(this);
    this.rearrangeChildren();
}

Grid.prototype.replaceChild = function(n, o)
{
    var cell = document.createElement("td");
    cell.appendChild(n.getDOMNode());
    var index = this.children.indexOf(o);
    this.cells[index] = cell;
    this.o.getDOMNode().parent.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

Grid.prototype.removeChild = function(t)
{
    var index = this.children.indexOf(t);
    this.cells.splice(index, 1);
    t.setParent(null);
    this.rearrangeChildren();
}


module.exports = Grid;
