"use strict";
var Prolog = require('../lib/proscript2/src/core.js');
var PrologEngine = require('./prolog_engine');
var ReactWidget = require('./react_widget');
var PrologState = require('./prolog_state');

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


module.exports = {render: render};
