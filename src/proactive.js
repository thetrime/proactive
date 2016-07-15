"use strict";
var Prolog = require('../lib/proscript2/build/proscript.js');
var PrologEngine = require('./prolog_engine');
var ReactWidget = require('./react_widget');
var PrologState = require('./prolog_state');

function render(url, rootElementId, container)
{
    var engine = new PrologEngine(url, rootElementId, function()
                                  {
                                      var widget = new ReactWidget(null, engine, rootElementId, PrologState.emptyState).getDOMNode();
                                      console.log("Finished with root widget");
                                      container.className += " proactive_container vertical_layout vertical_fill horizontal_fill";
                                      container.appendChild(widget);
                                  });
}


module.exports = {render: render};
