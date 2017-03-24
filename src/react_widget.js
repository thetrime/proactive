"use strict";

var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants');
var global_widget_id = 0;

function ReactWidget(parentContext, engine, elementId, props, callback)
{
    ReactComponent.call(this);
    this.engine = engine;
    // Suppose a widget fires an event which results in the widget no longer existing.
    // Once the handler has completed, it will try and set the state on the (long since cleaned-up) widget
    // which will result in freeing (a second time) the state, and therefore a mess
    // To manage this, we have an explicit variable here: this.destroyed, which is set to true to indicate that
    // setState should be a NOP since the widget has been destroyed
    this.destroyed = false;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;
    this.vDom = null;
    this.internalComponent = null;
    this.id = "$widget" + (global_widget_id++);
    engine.registerWidget(this);
    this.setProperties(props.getProperties());
    this.listeners = Constants.emptyListAtom;
    engine.getInitialState(this, elementId, props, function(state)
                           {
                               this.state = state;
                               this.engine.render(this, this.elementId, this.state, this.props, function(vDom)
                                                  {
                                                      this.vDom = vDom;
                                                      engine.createElementFromVDom(this.vDom, this, function(internalComponent)
                                                                                   {
                                                                                       this.internalComponent = internalComponent;
                                                                                       this.internalComponent.setOwnerDocument(this);
                                                                                       //this.hasFluxListeners = engine.checkForFluxListeners(this);
                                                                                       this.internalComponent.restyle();
                                                                                       this.engine.checkForMessageHandlers(this,
                                                                                                                           function()
                                                                                                                           {
                                                                                                                               callback(this);
                                                                                                                           }.bind(this))
                                                                                       //callback.bind(this)(this);
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
    if (this.destroyed)
    {
        callback();
        return;
    }
    if (this.state != undefined && !this.state.is_global)
    {
        Prolog._release_blob("state", this.state.blob);
    }
    this.state = newState;
    this.engine.checkForMessageHandlers(this,
                                        function()
                                        {
                                            this.reRender(callback);
                                        }.bind(this))
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
    console.log("Destroy component: " + Prolog._portray(vNode));
    this.engine.deregisterWidget(this);
    this.freeComponent(vNode);
}

ReactWidget.prototype.freeComponent = function(vNode)
{
    this.engine.deregisterWidget(this);
    this.state.freeState();
    this.props.freeState();
    Prolog._release_blob("react_component", this.blob);
    this.destroyed = true;
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

ReactWidget.prototype.queueEvent = function(handler, event, callback)
{
    this.engine.queueEvent(handler, event, this, callback);
}


ReactWidget.prototype.debugStuff = function()
{
    this.engine.debugStuff();
}

module.exports = ReactWidget;
