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
    this.weights = [0];
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
        if (!(newWeights.length == this.weights.length && newWeights.every(function(v,i) { return v === this.weights[i]})))
            must_relayout = true;
        this.weights = newWeights;
    }
    if (must_relayout)
        this.relayout();
}

Grid.prototype.relayout = function(t)
{
    while (this.table.rows.length != 0)
        this.table.deleteRow(-1)
    for (var i = 0; i < this.children.length; i++)
        this.appendChild(this.children[i]);
}

Grid.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    if (this.table.rows.length == 0)
    {
        var row = document.createElement("tr");
        if (this.weights[0] == 0)
            cell.className = 'grid-shrink';
        else
            cell.className = 'grid-expand';
        row.appendChild(cell);
        this.table.appendChild(row);
    }
    else
    {
        var lastRow = this.table.rows[this.table.rows.length-1];
        if (lastRow.childNodes.length == this.weights.length)
        {
            var row = document.createElement("tr");
            if (this.weights[0] == 0)
                cell.className = 'grid-shrink';
            else
                cell.className = 'grid-expand';
            row.appendChild(cell);
            this.table.appendChild(row);
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


Grid.prototype.insertBefore = function(t, s)
{
    this.table.insertBefore(t.getDOMNode(), s.getDOMNode());
    var index;
    if (s == null)
        index = -1;
    else
        index = this.children.indexOf(s);
    // Relayout, adding the new one in when appropriate
    while (this.table.firstChild != null)
        this.table.firstChild.remove();
    var added = false;
    for (var i = 0; i < this.children.length; i++)
    {
        if (index < i && !added)
        {
            this.appendChild(t);
            added = true
        }
        this.appendChild(this.children[i]);
    }
    t.setParent(this);
}

Grid.prototype.replaceChild = function(n, o)
{
    // First locate the cell
    var index = this.children.indexOf(o);
    var row = Math.floor(index / this.weights.length)
    var col = index % this.weights.length;
    var rowDOM = this.table.rows[row];
    rowDOM.childNodes[col].replaceChild(rowDOM.childNodes[col].firstChild, n.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

Grid.prototype.removeChild = function(t)
{
    // First locate the cell
    var index = this.children.indexOf(t);
    var row = Math.floor(index / this.weights.length)
    var col = index % this.weights.length;
    var rowDOM = this.table.rows[row];
    rowDOM.removeChild(rowDOM.childNodes[col]);
    t.setParent(null);
    //this.relayout(); This is too hard. It relies on this.children being already updated, and since it is almost always wrong to remove a single element
    //                 anyway, I can live with the mess that you get if you try it
}

module.exports = Grid;
