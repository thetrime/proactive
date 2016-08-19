"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');

module.exports = {emptyListAtom: Prolog._make_atom("[]"),
                  curlyFunctor: Prolog._make_functor(Prolog._make_atom("{}"), 1),
                  curlyAtom: Prolog._make_atom("{}"),
                  listAtom : Prolog._make_atom("list"),
                  gluableAtom: Prolog._make_atom("gluable"),
                  atomAtom: Prolog._make_atom("atom"),
                  blobAtom: Prolog._make_atom("blob"),
                  attributeAtom: Prolog._make_atom("attribute"),
                  colonFunctor : Prolog._make_functor(Prolog._make_atom(":"), 2),
                  equalsFunctor : Prolog._make_functor(Prolog._make_atom("="), 2),
                  typeErrorFunctor: Prolog._make_functor(Prolog._make_atom("type_error"), 2),
                  errorFunctor: Prolog._make_functor(Prolog._make_atom("error"), 2),
                  thisFunctor : Prolog._make_functor(Prolog._make_atom("$this"), 2),
                  commaFunctor : Prolog._make_functor(Prolog._make_atom(","), 2),
                  cutFunctor : Prolog._make_functor(Prolog._make_atom("cut"), 1),
                  exceptionFunctor : Prolog._make_functor(Prolog._make_atom("exception"), 1),
                  crossModuleCallFunctor : Prolog._make_functor(Prolog._make_atom(":"), 2),
                  listFunctor : Prolog._make_functor(Prolog._make_atom("."), 2),
                  prologStateAtom : Prolog._make_atom("prolog_state"),
                  prologStateKeyAtom : Prolog._make_atom("prolog_state_key"),
                  abortedAtom: Prolog._make_atom("$aborted"),
                  nullAtom: Prolog._make_atom("null"),
                  nodeAtom: Prolog._make_atom("node"),
                  trueAtom: Prolog._make_atom("true")};
