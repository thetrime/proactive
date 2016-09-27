var ReactComponent = require('./react_component');
var Prolog = require('proscript');

var global_tab_id = 0;

function Tab()
{
    ReactComponent.call(this);
    this.baseClassName = "not_selected_tab";
    this.setDOMNode(document.createElement("div"));
    this.title = "";
    global_tab_id++;
}

Tab.prototype = new ReactComponent;

Tab.prototype.setSelected = function(t)
{
    console.log("Setting selected");
    this.input.checked = true;
}

Tab.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.title = Prolog._portray(t.label);
}

Tab.prototype.setTabpaneOwner = function(ownerId)
{
    this.input.name = ownerId;
}

Tab.prototype.getTitle = function()
{
    return this.title;
}

module.exports = Tab;
