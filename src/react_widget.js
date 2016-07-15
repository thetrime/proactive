"use strict";

var ReactComponent = require('./react_component');

function ReactWidget(parentContext, engine, elementId, props)
{
    this.engine = engine;
    this.elementId = elementId;
    this.props = props;
    this.owner = parentContext;

    this.setProperties(props.getProperties())
    // FIXME: Create a CodeChangeListener
    this.state = engine.getInitialState(elementId, props);
    this.vDom = engine.render(this, this.elementId, this.state, this.props);
    this.internalComponent = engine.createElementFromVDom(this.vDom, this);
    this.internalComponent.setOwnerDocument(this);
    this.hasFluxListeners = engine.checkForFluxListeners(this);
}

ReactWidget.prototype = new ReactComponent;

ReactWidget.prototype.getDOMNode = function()
{
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

ReactWidget.prototype.reRender = function()
{
    console.log("About to re-render");
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
