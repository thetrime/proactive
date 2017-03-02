var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Grid()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    this.baseClassName = "grid"
    var node = document.createElement("div");
    node.appendChild(this.table);
    this.setDOMNode(node);
    this.rows = [];
    this.column_count = 1;
    this.weights = [0];
}
Grid.prototype = new ReactComponent;

Grid.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    var must_relayout = false;
    if (t.columns !== undefined)
    {
        var old_count = this.column_count;
        this.column_count = ReactComponent.numericValueOr(t.columns, 1);
        if (old_count != this.column_count)
            must_relayout = true;
    }
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
        if (!(newWeights.length == this.weights.length && newWeights.every(function(v,i) { return v === this.weights[i]})))
            must_relayout = true;
        this.weights = newWeights;
    }
    if (must_relayout)
        this.relayout();
}

Grid.prototype.relayout = function(t)
{
    while (this.table.firstChild != null)
        this.table.firstChild.remove();
    for (var i = 0; i < this.children.length; i++)
        this.appendChild(this.children(i));
}

Grid.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    if (this.rows.length == 0)
    {
        var row = document.createElement("tr");
        if (this.weights[0] == 0)
            cell.className = 'grid-shrink';
        else
            cell.className = 'grid-expand';
        row.appendChild(cell);
        this.table.appendChild(row);
        this.rows.push(row);
    }
    else
    {
        var lastRow = this.rows[this.rows.length-1];
        if (lastRow.childNodes.length == this.column_count)
        {
            var row = document.createElement("tr");
            if (this.weights[0] == 0)
                cell.className = 'grid-shrink';
            else
                cell.className = 'grid-expand';
            row.appendChild(cell);
            this.table.appendChild(row);
            this.rows.push(row);
        }
        else
        {
            if (this.weights.length < lastRow.childNodes.length)
                cell.className = 'grid-shrink';
            else
            {
                var weight = this.weights[lastRow.childNodes.length];
                if (weight == 0)
                    cell.className = 'grid-shrink';
                else
                    cell.className = 'grid-expand';
            }
            lastRow.appendChild(cell);
        }
    }
    t.setParent(this);
}

/*
Grid.prototype.insertBefore = function(t, s)
{
    this.table.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

Grid.prototype.replaceChild = function(n, o)
{
    this.table.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}


Grid.prototype.removeChild = function(t)
{
    this.table.removeChild(t.getDOMNode());
    t.setParent(null);
}
*/

module.exports = Grid;
