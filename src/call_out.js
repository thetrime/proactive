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
    if (t['for'] != undefined)
    {
        var target_id = Prolog._portray(t['for']);
        window.setTimeout(function()
                          {
                              var f = document.getElementById(target_id);
                              if (f == null)
                              {
                                  console.log("Warning: CallOut for " + target_id + " but there is no element with that ID in the DOM");
                                  return;
                              }
                              var rect = f.getBoundingClientRect();
                              console.log(rect); // 520x1600
                              // This is still not exactly right - if the parent is itself positioned, we have a problem
                              this.getDOMNode().style.top = (rect.top + f.clientHeight + 15) + 'px';
                              this.getDOMNode().style.left = rect.left + (rect.width/2) - (this.getDOMNode().clientWidth/2) + 'px';
                              this.getDOMNode().style.visibility = 'visible';
                              this.getDOMNode().style.opacity = '1';

                          }.bind(this), 0);
    }
}

module.exports = CallOut;
