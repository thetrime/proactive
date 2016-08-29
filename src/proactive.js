"use strict";
var PrologEngine = require('./prolog_engine');
var ReactWidget = require('./react_widget');
var PrologState = require('./prolog_state');
var Constants = require('./constants');
var Prolog = require('../lib/proscript2/build/proscript.js');
var ReactComponent = require('./react_component.js');
var ProactiveComponentFactory = require('./proactive_component_factory.js');

function render(url, rootElementId, container, callback)
{
    var engine = new PrologEngine(url, rootElementId, function()
                                  {
                                      new ReactWidget(null, engine, rootElementId, PrologState.emptyState, function(widget)
                                                      {
                                                          container.className += " proactive_container vertical_layout vertical_fill horizontal_fill";
                                                          container.appendChild(widget.getDOMNode());
                                                          callback();
                                                      })
                                  });
}


module.exports = {render: render,
                  registerComponent: ProactiveComponentFactory.registerComponent,
                  ReactComponent: ReactComponent,
                  Constants: Constants,
                  make_atom: Prolog._make_atom,
                  _qqqx: Prolog._qqq};
