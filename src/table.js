var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');
var TableFooter = require('./table_footer');
var Row = require('./row');

// We have to use javascript to set the grid-template-columns. This is ugly and inefficient.
// This could be very tidily fixed using display: subgrid, if only someone would implement it >_<

function Table()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    this.baseClassName = "react_table scrollpane";
    this.setDOMNode(this.table);
    this.thead = document.createElement("thead");
    this.tbody = document.createElement("tbody");
    this.tfoot = document.createElement("tfoot");
    this.table.appendChild(this.thead);
    this.table.appendChild(this.tbody);
    this.table.appendChild(this.tfoot);
    this.dirty = false;
    this.dirtCount = 0;
    this.markDirty();
}
Table.prototype = new ReactComponent;

Table.prototype.markDirty = function()
{
    var count = this.thead.children.length;
    if (count == this.dirtCount && this.dirty)
        return;
    this.dirtCount = count;
    var style = "";
    for (var i = 0; i < count; i++)
        style +="auto ";
    this.thead.style['grid-template-columns'] = style;
    this.tfoot.style['grid-template-columns'] = style;
    this.tbody.style['grid-template-columns'] = style;
    if (!this.dirty)
    {
        // Only put in one request to resize. If the number of columns changes that is fine - we will redo the auto style in the lines just
        // above, but we won't put in another request for relayout() here
        window.requestAnimationFrame(function() {this.relayout();}.bind(this));
    }
    this.dirty = true;

}

Table.prototype.relayout = function()
{
    var headerRow = this.thead.firstChild;
    var firstRow = this.tbody.firstChild;
    var firstFoot = this.tfoot.firstChild;
    var padding = 3;

    // First, unfortunately, we need to look at every single cell
    var columns = [];
    var count = this.thead.children.length;
    // Start by setting the size of each column to the size of the header
    for (var i = 0; i < this.thead.children.length; i++)
        columns[i] = this.thead.children[i].clientWidth;
    // Then for every cell, if it is wider than the current column, widen the column
    for (var j = 0; j < this.tbody.children.length; j++)
    {
        if (columns[j % count] < this.tbody.children[j].clientWidth)
            columns[j % count] = this.tbody.children[j].clientWidth;
    }
    // Finally, do the footer
    for (var j = 0; j < this.tfoot.children.length; j++)
        if (columns[j % count] < this.tfoot.children[j].clientWidth)
            columns[j % count] = this.tfoot.children[j].clientWidth;

    var total = 0;
    for (var i = 0; i < columns.length; i++)
    {
        total += columns[i];
    }

    var style = '';
    for (var i = 0; i < columns.length; i++)
        style += (100 * columns[i] / total) + "% ";

    this.thead.style['grid-template-columns'] = style;
    this.tfoot.style['grid-template-columns'] = style;
    this.tbody.style['grid-template-columns'] = style;
    this.table.style['min-width'] = (total + count * padding) + "px";
    this.dirty = false;
}


Table.prototype.appendChild = function(t)
{
    if (t instanceof TableHeader)
    {
        var row = t.getElements();
        for (var i = 0; i < row.length; i++)
            this.thead.appendChild(row[i]);
    }
    else if (t instanceof TableFooter)
    {
        var row = t.getElements();
        for (var i = 0; i < row.length; i++)
            this.tfoot.appendChild(row[i]);
    }
    else if (t instanceof Row)
    {
        var row = t.getElements();
        for (var i = 0; i < row.length; i++)
            this.tbody.appendChild(row[i]);
    }
    t.setParent(this);
    this.markDirty()
}

/* FIXME: Implement!
Table.prototype.insertBefore = function(t, s)
{
    this.table.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

Table.prototype.replaceChild = function(n, o)
{
    this.table.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

Table.prototype.removeChild = function(t)
{
    this.table.removeChild(t.getDOMNode());
    t.setParent(null);
}
*/

module.exports = Table;
