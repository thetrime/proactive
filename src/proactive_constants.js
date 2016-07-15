"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');

module.exports.gluableAtom = new Prolog.AtomTerm("gluable");
module.exports.attributeAtom = new Prolog.AtomTerm("attribute");
module.exports.colonFunctor = new Prolog.Functor(new Prolog.AtomTerm(":"), 2);
module.exports.equalsFunctor = new Prolog.Functor(new Prolog.AtomTerm("="), 2);
module.exports.thisFunctor = new Prolog.Functor(new Prolog.AtomTerm("$this"), 2);

