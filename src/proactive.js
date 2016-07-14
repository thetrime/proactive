"use strict";
var Prolog = require('../lib/proscript2/build/proscript.js');
var PrologEngine = require('./prolog_engine');
var ReactWidget = require('./react_widget');
var PrologState = require('./prolog_state');

function render(url, rootElementId, container)
{
    var engine = new PrologEngine(url, rootElementId, function()
                                  {
                                      container.appendChild(new ReactWidget(null, engine, rootElementId, PrologState.emptyState).getDOMNode());
                                  });
}


module.exports = {render: render};
