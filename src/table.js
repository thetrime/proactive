var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');
var TableFooter = require('./table_footer');
var Row = require('./row');

// We have to use javascript to set the grid-template-columns. This is pretty inefficient.
// I think that this could be very tidily fixed using display: subgrid, if only someone would implement it >_<

// The implementation here is quite complex but it seems to work. The gist is that we use three css-grid blocks to
// represent the table. The items in the header, footer and body must all be flattened - since display:contents is
// still very new (only Firefox supports it right now), in order for things to be aligned in the grid, they have to
// be direct children. To facilitate this, Row, TableHeader and TableFooter all support a method getElements() which
// returns a list of their child DOM elements.
// This list is (appears to be?) live, so changing the elements in the Row will be reflected automatically in the
// DOM for the table itself.

// Problems:
//   * When you change a cell containing (say) a Label, we do not get a notification to relayout. The only way to
//     fix this is to add in a markDirty() call to everything and propagate all changes backwards. This sounds really
//     expensive :( (either that, or poll. Even more horrifying)
//   * I'm not convinced that adding a row will work properly

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

Table.prototype.insertBefore = function(t, s)
{
    t.setParent(this);
}

Table.prototype.replaceChild = function(n, o)
{
    n.setParent(this);
    o.setParent(null);
}

Table.prototype.removeChild = function(t)
{
    t.setParent(null);
}


module.exports = Table;
