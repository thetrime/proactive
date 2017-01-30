var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Row()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tr"));
}

Row.prototype = new ReactComponent;
Row.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    this.domNode.appendChild(cell);
}

Row.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.onDblClick !== undefined)
        this.setDblClickHandler(t.onDblClick);
}

Row.prototype.setDblClickHandler = function(value)
{
    if (this.dblClickHandler != null)
        Prolog._free_local(this.dblClickHandler);
    if (ReactComponent.isNull(value))
    {
        if (this.domNode.onDblClick !== undefined)
            this.domNode.onDblClick = undefined;
        return;
    }
    this.dblClickHandler = Prolog._make_local(value);
    this.domNode.ondblclick = dblClickHandler.bind(this);
}

function dblClickHandler(event)
{
    this.getOwnerDocument().triggerEvent(this.dblClickHandler, Constants.emptyListAtom, function() {});
}


module.exports = Row;
