var ReactComponent = require('./react_component');
var Panel = require('./panel');
var Prolog = require('proscript');
var Constants = require('./constants.js');

// A CallOut is a special kind of Panel that has a z-index of 1 more than its parent and is absolutely positioned

function CallOut()
{
    Panel.call(this);
    this.baseClassName = 'callout proactive_container';
    this.restyle();
}

CallOut.prototype = new Panel;

CallOut.prototype.setProperties = function(t)
{
    Panel.prototype.setProperties.call(this, t);
}

CallOut.prototype.setParent = function(t)
{
    ReactComponent.prototype.setParent.call(this, t);
    if (t !== null)
        window.setTimeout(function()
                          {
                              var rect = t.getDOMNode().getBoundingClientRect();
                              // This is the visible offset from the nearest positioned ancestor
                              var ancestorRect = t.getDOMNode().offsetParent.getBoundingClientRect();
                              // This is still not exactly right - if the parent is itself positioned, we have a problem
                              this.getDOMNode().style.top = (rect.top + t.getDOMNode().clientHeight + 5 - ancestorRect.top) + 'px';
                              this.getDOMNode().style.left = rect.left + (rect.width/2) - (this.getDOMNode().clientWidth/2) + 'px';
                              this.getDOMNode().style.visibility = 'visible';

                          }.bind(this), 0);
}

module.exports = CallOut;
