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

module.exports = ReactWidget;
