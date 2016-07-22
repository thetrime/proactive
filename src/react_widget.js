"use strict";

var ReactComponent = require('./react_component');

function ReactWidget(parentContext, engine, elementId, props, callback)
{
    this.engine = engine;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;

    this.internalComponent = null;

    this.setProperties(props.getProperties());
    // FIXME: Create a CodeChangeListener
    /*
    console.log("Creating widget " + elementId + " with props " + props);
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
    this.engine.render(this, this.elementId, this.state, this.props, function(newVDom)
                       {
                           this.engine.diff(this.vDom, newVDom.dereference(), function(patches)
                                            {
                                                this.engine.applyPatch(patches, this.internalComponent, function()
                                                                       {
                                                                           this.internalComponent.setOwnerDocument(this);
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

module.exports = ReactWidget;
