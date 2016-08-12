"use strict";

var ReactComponent = require('./react_component');
var Prolog = require('../lib/proscript2/build/proscript.js');

function ReactWidget(parentContext, engine, elementId, props, callback)
{
    ReactComponent.call(this);
    this.engine = engine;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;
    this.vDom = null;
    this.internalComponent = null;

    this.setProperties(props.getProperties());
    // FIXME: Create a CodeChangeListener
    //console.log("Creating widget " + elementId + " with props " + props);
    /*
    try { throw new Error()} catch(qxy) {console.log("Stack depth: " + qxy.stack.split('\n').length);}
    console.log("Getting initial state for " + elementId + " with props " + props);
*/
    engine.getInitialState(elementId, props, function(state)
                           {
                               this.state = state;
                               this.engine.render(this, this.elementId, this.state, this.props, function(vDom)
                                                  {
                                                      this.vDom = vDom;
                                                      engine.createElementFromVDom(this.vDom, this, function(internalComponent)
                                                                                   {
                                                                                       this.internalComponent = internalComponent;
                                                                                       this.internalComponent.setOwnerDocument(this);
                                                                                       this.hasFluxListeners = engine.checkForFluxListeners(this);
                                                                                       this.internalComponent.restyle();
                                                                                       setTimeout(function(){callback(this)}.bind(this), 0);
                                                                                   }.bind(this));
                                                  }.bind(this));
                           }.bind(this));
}

ReactWidget.prototype = new ReactComponent;

ReactWidget.prototype.getDOMNode = function()
{
    if (this.internalComponent == null)
        return null;
    return this.internalComponent.getDOMNode();
}

ReactWidget.prototype.getEngine = function()
{
    return this.engine;
}

ReactWidget.prototype.getState = function()
{
    return this.state;
}

ReactWidget.prototype.getProps = function()
{
    return this.props;
}

ReactWidget.prototype.getComponentName = function()
{
    return this.elementId;
}

ReactWidget.prototype.setState = function(newState, callback)
{
    this.state = newState;
    console.log("The state of " + this.elementId + " has been set to " + this.state.toString());
    this.reRender(callback);
}

ReactWidget.prototype.updateWidget = function(newProps, callback)
{
    this.props = newProps;
    this.engine.componentWillReceiveProps(this.elementId, this,
                                          function(didReceive)
                                          {
                                              if (!didReceive)
                                                  this.reRender(callback);
                                              else
                                                  callback(this);
                                          }.bind(this));
}


ReactWidget.prototype.reRender = function(callback)
{
    console.log("Re-rendering: " + this.state.toString());
    this.engine.render(this, this.elementId, this.state, this.props, function(newVDom)
                       {
                           console.log("re-diff");
                           this.engine.diff(this.vDom, newVDom, function(patches)
                                            {
                                                console.log("re-patch");
                                                this.engine.applyPatch(patches, this.internalComponent, function()
                                                                       {
                                                                           console.log("all done");
                                                                           this.internalComponent.setOwnerDocument(this);
                                                                           if (this.vDom != null)
                                                                               Prolog._free_local(this.vDom);
                                                                           this.vDom = newVDom;
                                                                           //console.log("All patches applied");
                                                                           callback(this);
                                                                       }.bind(this));
                                            }.bind(this));
                       }.bind(this));
}



ReactWidget.prototype.triggerEvent = function(handler, event, callback)
{
    this.engine.triggerEvent(handler, event, this, callback);
}

ReactWidget.prototype.debugStuff = function()
{
    this.engine.debugStuff();
}

module.exports = ReactWidget;
