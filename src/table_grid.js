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

Grid.prototype.restyle = function()
{
    ReactComponent.prototype.restyle.call(this);
    // Horizontal grids can only be stretched if a maxWidth is provided.
    if (this.maxWidth != null && this.fill == "horizontal")
        this.getDOMNode().style.width = '100%';
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
        else
        {
            // I have no idea why this works, but what I am trying to emulate is width="0*". Chrome seems to ignore 0*
            // (after all, it is not part of HTML5) but 10px generates something that does the same thing - a column that
            // is just wide enough for the contents. Note that this is always wider than 10px...
            col.width = '10px';
        }
    }
}

Grid.prototype.debug = function()
{
    console.log("Current cells:");
    for (var i = 0; i < this.cells.length; i++)
        console.log("Cell " + i + ": " + this.cells[i].outerHTML);
    console.log("-----");
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
    // o.getDOMNode().parentNode is the TD holding the parent
    // o.getDOMNode().parentNode.parentNode is the TR holding the cell
    o.getDOMNode().parentNode.parentNode.replaceChild(cell, o.getDOMNode().parentNode);
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
