var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');
var TableFooter = require('./table_footer');
var Row = require('./row');
var Prolog = require('proscript');
var Constants = require('./constants.js');
// This is the third major attempt at tables with fixed headers and footers.
// What a nightmare. For something so obvious, it is amazing how hard it is to implement in HTML.
// After all, why have a tbody separate from the thead and tfoot if you cant scroll the tbody?

function Table()
{
    ReactComponent.call(this);
    this.container = document.createElement("div");
    this.header_container = document.createElement("div");
    this.header_container.className = "react_header_container";
    this.footer_container = document.createElement("div");
    this.footer_container.className = "react_footer_container";
    this.header_table = document.createElement("table");
    this.header_table.className = "react_header_table";
    this.table = document.createElement("table");
    this.footer_table = document.createElement("table");
    this.footer_table.className = "react_footer_table";
    this.baseClassName = "react_table scrollpane ";
    this.setDOMNode(this.container);
    this.container.appendChild(this.header_container);
    this.header_container.appendChild(this.header_table);
    this.body_container = document.createElement("div");
    this.body_container.appendChild(this.table);
    this.body_container.className = "react_table_body";
    this.container.appendChild(this.body_container);
    this.footer_container.appendChild(this.footer_table);
    this.container.appendChild(this.footer_container);
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
    this.weights = null;
    this.markDirty();
}
Table.prototype = new ReactComponent;

Table.prototype.set_column_width = function(w)
{
    var col = document.createElement("col");
    col.style.width = w;
    this.header_colgroup.appendChild(col);
    col = document.createElement("col");
    col.style.width = w;
    this.body_colgroup.appendChild(col);
    col = document.createElement("col");
    col.style.width = w;
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
    this.header_table.style['table-layout'] = 'auto';
    this.table.style['table-layout'] = 'auto';
    this.footer_table.style['table-layout'] = 'auto';

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

    var columns = [];

    if (this.weights != null)
    {
        columns = this.weights;
    }
    else
    {
        // Otherwise, unfortunately, we need to look at every single cell
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
    }

    var total = 0;
    for (var i = 0; i < columns.length; i++)
    {
        total += columns[i];
    }
    this.remove_colgroups();
    for (var i = 0; i < columns.length; i++)
    {
        this.set_column_width((100 * columns[i] / total) + '%');
    }
    // Reset the width on the tables to take up the full width of the panel
    this.header_table.style.width = '';
    this.table.style.width = '';
    this.footer_table.style.width = '';
//    this.header_table.style['table-layout'] = 'fixed';
//    this.table.style['table-layout'] = 'fixed';
//    this.footer_table.style['table-layout'] = 'fixed';

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

Table.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.weights !== undefined)
    {
        if (ReactComponent.isNull(t.weights))
        {
            this.weights = null;
            this.markDirty();
        }
        else
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
            if (this.weights == null || newWeights.length != this.weights.length || !newWeights.every(function(v,i) { return v === this.weights[i]}.bind(this)))
                this.markDirty();
            this.weights = newWeights;
        }
    }
}

Table.prototype.insertBefore = function(t, s)
{
    var targetClass;
    var target;
    if (t instanceof TableHeader)
    {
        targetClass = TableHeader;
        target = this.thead;
    }
    else if (t instanceof Row)
    {
        targetClass = Row;
        target = this.tbody;
    }
    else if (t instanceof TableFooter)
    {
        targetClass = TableFooter;
        target = this.tfoot;
    }
    var sibling = null;
    if (s != null)
    {
        for (var i = this.children.indexOf(s); i < this.children.length; i++)
        {
            if (this.children[i] instanceof targetClass)
            {
                sibling = this.children[i].getDOMNode();
                break;
            }
        }
    }
    target.insertBefore(t.getDOMNode(), sibling);
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
    else
    {
        // This might seem relatively simple, but remember we can easily get cases like this:
        // <Table>
        //   <Row/>
        //   <TableFooter/>    <-- this is what must be converted from TableFooter -> Row
        //   <Row/>
        //   <TableFooter/>
        // </Table>
        //
        // This means we cannot assume, for example, that a TableFooter child always appears after all the Row children

        // Out with the old, ...
        if (o instanceof TableHeader)
            this.thead.removeChild(o.getDOMNode());
        else if (o instanceof Row)
            this.tbody.removeChild(o.getDOMNode());
        else if (o instanceof TableFooter)
            this.tfoot.removeChild(o.getDOMNode());

        // In with the new
        // We must find the sibling to insert before. This is the next object in the child list with the same class as n (or null if there is no such child)
        // I did not know you could do this in Javascript until today!
        var targetClass;
        var target;
        if (n instanceof TableHeader)
        {
            targetClass = TableHeader;
            target = this.thead;
        }
        else if (n instanceof Row)
        {
            targetClass = Row;
            target = this.tbody;
        }
        else if (n instanceof TableFooter)
        {
            targetClass = TableFooter;
            target = this.tfoot;
        }
        var sibling = null;
        for (var i = this.children.indexOf(o); i < this.children.length; i++)
        {
            if (this.children[i] instanceof targetClass)
            {
                sibling = this.children[i].getDOMNode();
                break;
            }
        }
        target.insertBefore(n.getDOMNode(), sibling);
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
