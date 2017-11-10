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

// The zebra-striping now becomes very hard. There is no way I could see to style the first N cells in a grid, leave
// the following N alone, and then style the N after that. I could have applied classes to appropriate cells but then
// what happens if they move? We have to re-number then all. Nasty.

// So instead, we do something else that is also quite nasty - we dynamically create CSS and use nth-child rules to
// style every second row. This requires one row per column (as far as I could tell). When the table changes shape
// we only need to rewrite the N rules. To facilitate this, each tbody gets a 'unique' ID made of zebra_ and a counter

var uuid = 0;

function Table()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    this.uuid = "zebra_" + (uuid++);
    this.baseClassName = "react_table scrollpane ";
    this.setDOMNode(this.table);
    this.thead = document.createElement("thead");
    this.tbody = document.createElement("tbody");
    this.tbody.className = this.uuid;
    this.tfoot = document.createElement("tfoot");
    this.table.appendChild(this.thead);
    this.table.appendChild(this.tbody);
    this.table.appendChild(this.tfoot);
    this.dirty = false;
    this.dirtCount = 0;
    this.column_count = 0;
    // Zebra-sheet
    var style = document.createElement("style");
    style.appendChild(document.createTextNode(""));
    document.head.appendChild(style);
    this.sheet = style.sheet;
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
    this.table.style['min-width'] = '';


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

    // recalibrate the zebra-striping rules if count has changed
    if (count != this.column_count)
    {
        var ruleCount = this.sheet.cssRules.length;
        for (var i = 0; i < ruleCount; i++)
            this.sheet.deleteRule(0);
        for (var i = 0; i < count; i++)
            this.sheet.insertRule('.' + this.uuid + ' td:nth-child(' + (2*count) + 'n - ' + i + ") { background: rgba(0,0,0,0.05) !important; }");
        this.column_count = count;
    }

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
    console.log("Table.insertBefore is not implemented");
    t.setParent(this);
}

Table.prototype.replaceChild = function(n, o)
{
    var count = this.thead.children.length;
    // This is quite complicated since there are 6 possible cases.
    if (n instanceof TableHeader && o instanceof TableHeader)
    {
        // This is probably the simplest case.
        // Delete the old thead and then add in the new one
        while (this.thead.hasChildNodes())
            this.thead.removeChild(this.thead.lastChild);
        var row = n.getElements();
        for (var i = 0; i < row.length; i++)
            this.thead.appendChild(row[i]);
    }
    else if (n instanceof TableFooter && o instanceof TableFooter)
    {
        // This is more challenging. We need to know WHICH footer we are replacing
        var footIndex = this.children.indexOf(o);
        // Now delete the count elements from tfoot starting at footIndex
        // Since the list will keep shrinking, we can just keep deleting the same index
        for (var i = 0; i < count; i++)
            this.tfoot.removeChild(this.tfoot.childNodes[footIndex]);
        // Now we can add them back in
        var sibling = this.tfoot.childNodes[footIndex];
        var row = n.getElements();
        for (var i = 0; i < row.length; i++)
            this.tfoot.insertBefore(row[i], sibling);
    }
    else if (n instanceof Row && o instanceof Row)
    {
        // This is basically the same as TableFooter -> TableFooter
        var bodyIndex = this.children.indexOf(o);
        for (var i = 0; i < count; i++)
            this.tbody.removeChild(this.tbody.childNodes[bodyIndex]);
        var sibling = this.tbody.childNodes[bodyIndex];
        var row = n.getElements();
        for (var i = 0; i < row.length; i++)
            this.tbody.insertBefore(row[i], sibling);
    }
    else if (n instanceof Row && o instanceof TableFooter)
    {
        // It may seem like the row has to be at the end here if we are making a TableFooter into a row,
        // but internally we can have:
        // <Table>
        //   <Row/>
        //   <TableFooter/>    <-- this is what must be converted from TableFooter -> Row
        //   <Row/>
        //   <TableFooter/>
        // </Table>

        var bodyIndex = 0;
        var footIndex = 0;
        for (var i = this.children.indexOf(o); i >= 0; i--)
        {
            if (this.children[i] instanceof Row)
                bodyIndex++;
            else if (this.children[i] instanceof TableFooter)
                footIndex++;
        }
        // We want to remove the (footIndex)th footer and insert a new row after the (bodyIndex)th row
        // The actual position of the first item in the footer is (footIndex * count) since a footer
        // describes (count) actual nodes
        for (var i = 0; i < count; i++)
            this.tfoot.removeChild(this.tfoot.childNodes[footIndex * count]);

        // Similarly, (bodyIndex*count) describes the index of the tbody that we want to insert before
        var row = n.getElements();
        var beforeSibling = this.tbody.childNodes[bodyIndex * count];
        for (var i = 0; i < row.length; i++)
            this.tbody.insertBefore(row[i], beforeSibling);
    }
    else if (n instanceof TableFooter && o instanceof Row)
    {
        // This is basically the same as the Row/TableFooter case above, just with the opposite operations at the end
        // The only wrinkle is that bodyIndex will always be one too high since this.children[i] is always Row
        var bodyIndex = -1;
        var footIndex = 0;
        for (var i = this.children.indexOf(o); i >= 0; i--)
        {
            if (this.children[i] instanceof Row)
                bodyIndex++;
            else if (this.children[i] instanceof TableFooter)
                footIndex++;
        }
        for (var i = 0; i < count; i++)
            this.tbody.removeChild(this.tbody.childNodes[bodyIndex * count]);
        var row = n.getElements();
        var beforeSibling = this.tfoot.childNodes[footIndex * count];
        for (var i = 0; i < row.length; i++)
            this.tfoot.insertBefore(row[i], beforeSibling);

    }
    else
    {
        console.log("Table transform not implemented");
        console.log(o);
        console.log("   ->   ");
        console.log(n);
    }
    n.setParent(this);
    o.setParent(null);
    this.markDirty()
}

Table.prototype.removeChild = function(t)
{
    // Unfortunately we cannot just remove the nth child of tfoot (for example)
    // by looking up which index in this.children t belongs at, because there is
    // no (simple) relationship between this.children and the actual DOM
    var count = this.thead.children.length;
    var bodyIndex = 0;
    var footIndex = 0;
    var headIndex = 0;
    for (var i = this.children.indexOf(t)-1; i >= 0; i--)
    {
        if (this.children[i] instanceof Row)
            bodyIndex++;
        else if (this.children[i] instanceof TableFooter)
            footIndex++;
        else if (this.children[i] instanceof TableHeader)
            headIndex++;
    }
    if (t instanceof Row)
    {
        for (var i = 0; i < count; i++)
            this.tbody.removeChild(this.tbody.childNodes[bodyIndex*count]);
    }
    else if (t instanceof TableFooter)
    {
        for (var i = 0; i < count; i++)
            this.tfoot.removeChild(this.tfoot.childNodes[footIndex*count]);
    }
    else if (t instanceof TableHeader)
    {
        while (this.thead.hasChildNodes())
            this.thead.removeChild(this.thead.lastChild);
    }
    t.setParent(null);
    this.markDirty()
}


module.exports = Table;
