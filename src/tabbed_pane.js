var ReactComponent = require('./react_component');
var Tab = require('./tab');

var global_tabpane_id = 0;

function TabbedPane()
{
    ReactComponent.call(this);
    this.tabpane_id = global_tabpane_id++;
    this.baseClassName = "tabs";
    this.setDOMNode(document.createElement("div"));
    this.layout = "vertical";
    this.tabBar = document.createElement("div");
    this.tabBar.className = "tab_bar";
    this.contentPane = document.createElement("div");
    this.domNode.appendChild(this.tabBar);
    this.domNode.appendChild(this.contentPane);
    this.tabs = [];
    this.tabButtons = [];
    this.currentIndex = -1;
}

TabbedPane.prototype = new ReactComponent;

TabbedPane.prototype.appendChild = function(t)
{
    if (t instanceof Tab)
    {
        //t.setTabpaneOwner(this.tabpane_id);
        var button = document.createElement("button");
        button.textContent = t.getTitle();
        button.onclick = function()
        {
            this.selectTab(t);
        }.bind(this);
        this.tabBar.appendChild(button);
        this.tabs.push(t);
        this.tabButtons.push(button);
        t.fill = this.fill;
        if (this.tabs.length == 1)
        {
            button.className = "selected";
            this.selectTab(t);
        }
    }
    ReactComponent.prototype.appendChild.call(this, t);
}

TabbedPane.prototype.setProperties = function(t)
{
    t.layout = undefined;
    ReactComponent.prototype.setProperties.call(this, t);
}

TabbedPane.prototype.selectTab = function(t)
{
    var index = this.tabs.indexOf(t);
    if (this.currentIndex != -1)
    {
        this.tabButtons[this.currentIndex].className = "not_selected";
        this.tabs[this.currentIndex].baseClassName = "not_selected_tab";
        this.tabs[this.currentIndex].restyle();
    }
    this.currentIndex = index;
    this.tabButtons[index].className = "selected"
    this.tabs[index].baseClassName = "selected_tab";
    this.tabs[index].restyle();
}

module.exports = TabbedPane;
