var ReactComponent = require('./react_component');

var global_tab_id = 0;

function Tab()
{
    ReactComponent.call(this);
    this.baseClassName = "proactive_container tab";
    this.setDOMNode(document.createElement("div"));
    this.input = document.createElement("input");
    this.input.type = "radio";
    this.input.style.display = "none";
    this.input.id = "tab_" + global_tab_id;
    this.label = document.createElement("label");
    this.label.htmlFor = "tab_" + global_tab_id;
    this.contentDiv = document.createElement("div");
    this.contentDiv.className = "content";
    this.domNode.appendChild(this.input);
    this.domNode.appendChild(this.label);
    this.domNode.appendChild(this.contentDiv);
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
        this.label.textContent = t.label;
}

Tab.prototype.setTabpaneOwner = function(ownerId)
{
    this.input.name = ownerId;
}

Tab.prototype.appendChild = function(t)
{
    this.contentDiv.appendChild(t.getDOMNode());
}
// FIXME: Implement other DOM methods!

module.exports = Tab;
