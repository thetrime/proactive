var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function Panel()
{
    ReactComponent.call(this);
    this.baseClassName = "proactive_container";
    this.setDOMNode(document.createElement("div"));
}



Panel.prototype = new ReactComponent;


Panel.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        this.legendElement = document.createElement("h1");
        var s = document.createElement("span");
        s.textContent = Prolog._portray(t.label);
        this.baseClassName = "proactive_container fieldset";
        this.legendElement.appendChild(s);
        this.domNode.insertBefore(this.legendElement, this.domNode.firstChild);
        this.restyle();
    }
    if (t.scroll !== undefined)
    {
        if (ReactComponent.isNull(t.scroll))
            this.baseClassName = "proactive_container";
        else if (Prolog._portray(t.scroll) == "both")
            this.baseClassName = "proactive_container scrollpane scroll";
        else if (Prolog._portray(t.scroll) == "horizontal")
            this.baseClassName = "proactive_container scrollpane scrollx";
        else if (Prolog._portray(t.scroll) == "vertical")
            this.baseClassName = "proactive_container scrollpane scrolly";
        this.restyle();
    }
}

module.exports = Panel;
