"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');

module.exports.gluableAtom = new Prolog.AtomTerm("gluable");
module.exports.attributeAtom = new Prolog.AtomTerm("attribute");
module.exports.colonFunctor = new Prolog.Functor(new Prolog.AtomTerm(":"), 2);
module.exports.equalsFunctor = new Prolog.Functor(new Prolog.AtomTerm("="), 2);
module.exports.thisFunctor = new Prolog.Functor(new Prolog.AtomTerm("$this"), 2);
module.exports.cutFunctor = new Prolog.Functor(new Prolog.AtomTerm("cut"), 1);
module.exports.exceptionFunctor = new Prolog.Functor(new Prolog.AtomTerm("exception"), 1);
module.exports.nullAtom = new Prolog.AtomTerm("null");
module.exports.nodeAtom = new Prolog.AtomTerm("node");
