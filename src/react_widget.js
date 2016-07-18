"use strict";

var ReactComponent = require('./react_component');

function ReactWidget(parentContext, engine, elementId, props)
{
    this.engine = engine;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;

    this.internalComponent = null;

    this.setProperties(props.getProperties());
    // FIXME: Create a CodeChangeListener
    this.state = engine.getInitialState(elementId, props);
    //console.log("Rendering: " + this.elementId);
    this.vDom = engine.render(this, this.elementId, this.state, this.props);
    //console.log("Done rendering: " + this.elementId);
    this.internalComponent = engine.createElementFromVDom(this.vDom, this);
    this.internalComponent.setOwnerDocument(this);
    this.hasFluxListeners = engine.checkForFluxListeners(this);
    this.internalComponent.restyle();
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

ReactWidget.prototype.setState = function(newState)
{
    this.state = newState;
    this.reRender();
}

ReactWidget.prototype.updateWidget = function(newProps)
{
    this.props = newProps;
    if (!this.engine.componentWillReceiveProps(this.elementId, this))
        this.reRender();
    return this;
}

ReactWidget.prototype.reRender = function()
{
    var newVDom = this.engine.render(this, this.elementId, this.state, this.props);
    var patches = this.engine.diff(this.vDom, newVDom.dereference());
    this.internalComponent = this.engine.applyPatch(patches, this.internalComponent);
    this.internalComponent.setOwnerDocument(this);
    this.vDom = newVDom;
}



ReactWidget.prototype.triggerEvent = function(handler, event)
{
    this.engine.triggerEvent(handler, event, this);
}

module.exports = ReactWidget;
