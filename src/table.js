var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');
var TableFooter = require('./table_footer');
var Row = require('./row');

// FIXME: This is the third major attempt. Hopefully this time?

function Table()
{
    ReactComponent.call(this);
    this.container = document.createElement("div");
    this.header_table = document.createElement("table");
    this.table = document.createElement("table");
    this.footer_table = document.createElement("table");
    this.baseClassName = "react_table scrollpane ";
    this.setDOMNode(this.container);
    this.container.appendChild(this.header_table);
    this.body_container = document.createElement("div");
    this.body_container.appendChild(this.table);
    this.body_container.className = "react_table_body";
    this.container.appendChild(this.body_container);
    this.container.appendChild(this.footer_table);
    this.thead = document.createElement("thead");
    this.tbody = document.createElement("tbody");
    this.tfoot = document.createElement("tfoot");
    this.header_colgroup = document.createElement("colgroup");
    this.body_colgroup = document.createElement("colgroup");
    this.footer_colgroup = document.createElement("colgroup");
    this.header_table.appendChild(this.header_colgroup);
    this.table.appendChild(this.body_colgroup);
    this.footer_table.appendChild(this.footer_colgroup);
    this.header_table.appendChild(this.thead);
    this.table.appendChild(this.tbody);
    this.footer_table.appendChild(this.tfoot);
    this.dirty = false;
    this.dirtCount = 0;
    this.column_count = 0;
    this.markDirty();
}
Table.prototype = new ReactComponent;

Table.prototype.set_column_width = function(p)
{
    var col = document.createElement("col");
    col.style = 'width: ' + p + '%';
    this.header_colgroup.appendChild(col);
    col = document.createElement("col");
    col.style = 'width: ' + p + '%';
    this.body_colgroup.appendChild(col);
    col = document.createElement("col");
    col.style = 'width: ' + p + '%';
    this.footer_colgroup.appendChild(col);
}

Table.prototype.remove_colgroups = function()
{
    while (this.header_colgroup.hasChildNodes())
        this.header_colgroup.removeChild(this.header_colgroup.lastChild);
    while (this.body_colgroup.hasChildNodes())
        this.body_colgroup.removeChild(this.body_colgroup.lastChild);
    while (this.footer_colgroup.hasChildNodes())
        this.footer_colgroup.removeChild(this.footer_colgroup.lastChild);
}

Table.prototype.markDirty = function()
{
    if (this.column_count == this.dirtCount && this.dirty)
        return;
    this.dirtCount = this.column_count;
    // We have to set the tables to all be 'auto' width here so that small columns arent stretched out
    // after all, we want to compute the minimum width for each column!
    this.header_table.style.width = 'auto';
    this.table.style.width = 'auto';
    this.footer_table.style.width = 'auto';
    this.remove_colgroups();
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
    var padding = 3;

    // First, unfortunately, we need to look at every single cell
    var columns = [];

    // Start by setting the size of each column to the size of the header
    for (var i = 0; i < this.column_count; i++)
        columns[i] = this.thead.lastChild.children[i].clientWidth;
    // Then for every cell, if it is wider than the current column, widen the column
    for (var j = 0; j < this.tbody.children.length; j++)
    {
        for (var i = 0; i < this.tbody.children[j].children.length; i++)
        {
            if (columns[i] < this.tbody.children[j].children[i].clientWidth)
                columns[i] = this.tbody.children[j].children[i].clientWidth;
        }
    }
    // Finally, do the footer
    for (var j = 0; j < this.tfoot.children.length; j++)
    {
        for (var i = 0; i < this.tfoot.children[j].children.length; i++)
        {
            if (columns[i] < this.tfoot.children[j].children[i].clientWidth)
                columns[i] = this.tfoot.children[j].children[i].clientWidth;
        }
    }

    var total = 0;
    for (var i = 0; i < columns.length; i++)
    {
        total += columns[i];
    }
    this.remove_colgroups();
    for (var i = 0; i < columns.length; i++)
    {
        this.set_column_width((100 * columns[i] / total));
    }
    // Reset the width on the tables to take up the full width of the panel
    this.header_table.style.width = '';
    this.table.style.width = '';
    this.footer_table.style.width = '';

    this.dirty = false;
}

Table.prototype.appendChild = function(t)
{
    if (t instanceof TableHeader)
    {
        this.thead.appendChild(t.getDOMNode());
        this.column_count = t.children.length;
    }
    else if (t instanceof TableFooter)
    {
        this.tfoot.appendChild(t.getDOMNode());
    }
    else if (t instanceof Row)
    {
        this.tbody.appendChild(t.getDOMNode());
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
    // This is quite complicated since there are 6 possible cases.
    if (n instanceof TableHeader && o instanceof TableHeader)
    {
        this.thead.replaceChild(n.getDOMNode(), o.getDOMNode());
    }
    else if (n instanceof TableFooter && o instanceof TableFooter)
    {
        this.tfoot.replaceChild(n.getDOMNode(), o.getDOMNode());
    }
    else if (n instanceof Row && o instanceof Row)
    {
        this.tbody.replaceChild(n.getDOMNode(), o.getDOMNode());
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

        // First, remove the old node. This is quite easy in a real table
        this.tfoot.removeChild(o.getDOMNode());
        // Now find the sibling to insert before. This is the next Row in the child list (or null if there is no such child)
        var sibling = null
        for (var i = this.children.indexOf(o); i < this.children.length; i++)
        {
            if (this.children[i] instanceof Row)
            {
                sibling = this.children[i].getDOMNode();
                break;
            }
        }
        this.tbody.insertBefore(n.getDOMNode(), sibling);
    }
    else if (n instanceof TableFooter && o instanceof Row)
    {
        // This is basically the same as the Row/TableFooter case above, just with the opposite operations at the end
        this.tbody.removeChild(o.getDOMNode());
        var sibling = null
        for (var i = this.children.indexOf(o); i < this.children.length; i++)
        {
            if (this.children[i] instanceof TableFooter)
            {
                sibling = this.children[i].getDOMNode();
                break;
            }
        }
        this.tfoot.insertBefore(n.getDOMNode(), sibling);

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
    if (t instanceof TableHeader)
        this.thead.removeChild(t.getDOMNode());
    if (t instanceof Row)
        this.tbody.removeChild(t.getDOMNode());
    if (t instanceof TableFooter)
        this.tfoot.removeChild(t.getDOMNode());

    t.setParent(null);
    this.markDirty()
}


module.exports = Table;
