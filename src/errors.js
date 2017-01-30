"use strict";

var Prolog = require('proscript');
var Constants = require('./constants.js');


module.exports.typeError = function(expected, actual)
{
    Prolog._set_exception(Prolog._make_compound(Constants.errorFunctor, [Prolog._make_compound(Constants.typeErrorFunctor, [expected, actual]), Prolog._make_variable()]));
    return 0;
}


module.exports.systemError = function(message)
{
    Prolog._set_exception(Prolog._make_compound(Constants.errorFunctor, [Prolog._make_compound(Constants.systemErrorFunctor, [message]), Prolog._make_variable()]));
    return 0;
}
