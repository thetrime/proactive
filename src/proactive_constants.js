"use strict";

var Prolog = require('../lib/proscript2/src/core.js');

module.exports.gluableAtom = Prolog.AtomTerm.get("gluable");
module.exports.attributeAtom = Prolog.AtomTerm.get("attribute");
module.exports.colonFunctor = Prolog.Functor.get(Prolog.AtomTerm.get(":"), 2);
module.exports.equalsFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("="), 2);
module.exports.thisFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("$this"), 2);
module.exports.cutFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("cut"), 1);
module.exports.exceptionFunctor = Prolog.Functor.get(Prolog.AtomTerm.get("exception"), 1);
module.exports.nullAtom = Prolog.AtomTerm.get("null");
module.exports.nodeAtom = Prolog.AtomTerm.get("node");
module.exports.trueAtom = Prolog.AtomTerm.get("true");
