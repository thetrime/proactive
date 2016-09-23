"use strict";

var ReactComponent = require('./react_component');
var Prolog = require('../lib/proscript2/build/proscript.js');

var global_widget_id = 0;

function ReactWidget(parentContext, engine, elementId, props, callback)
{
    ReactComponent.call(this);
    this.engine = engine;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;
    this.vDom = null;
    this.internalComponent = null;
    this.id = "$widget" + (global_widget_id++);

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
                                                                                       callback.bind(this)(this);
                                                                                       //setTimeout(function(){callback(this)}.bind(this), 0);
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
    if (this.state != undefined && !this.state.is_global)
    {
        Prolog._release_blob("state", this.state.blob);
    }
    this.state = newState;
    this.reRender(callback);
}

ReactWidget.prototype.updateWidget = function(newProps, callback)
{
    if (this.props != undefined)
        this.props.freeState();
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

ReactWidget.prototype.destroyWidget = function(vNode)
{
    console.log("ohai");
    this.freeComponent(vNode);
}

ReactWidget.prototype.freeComponent = function(vNode)
{
    this.state.freeState();
    this.props.freeState();
    Prolog._release_blob("react_component", this.blob);
    ReactComponent.prototype.freeComponent.call(this, vNode); //ie super.freeComponent(vNode)
}

ReactWidget.prototype.reRender = function(callback)
{
    this.engine.render(this, this.elementId, this.state, this.props, function(newVDom)
                       {
                           this.engine.diff(this.vDom, newVDom, function(patches)
                                            {
                                                this.engine.applyPatch(patches, this.internalComponent, function()
                                                                       {
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
