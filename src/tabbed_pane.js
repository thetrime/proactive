var ReactComponent = require('./react_component');
var Tab = require('./tab');

var global_tabpane_id = 0;

function TabbedPane()
{
    ReactComponent.call(this);
    this.tabpane_id = global_tabpane_id++;
    this.baseClassName = "tabs";
    this.setDOMNode(document.createElement("div"));
}

TabbedPane.prototype = new ReactComponent;

TabbedPane.prototype.appendChild = function(t)
{
    if (t instanceof Tab)
    {
        t.setTabpaneOwner(this.tabpane_id);
        if (this.domNode.childNodes.length == 0)
            t.setSelected();
    }
    ReactComponent.prototype.appendChild.call(this, t);
}

module.exports = TabbedPane;
