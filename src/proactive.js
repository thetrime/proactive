"use strict";

window.onPrologReady = function(Prolog)
{
    var ProactiveComponentFactory = require('./proactive_component_factory.js');
    var Constants = require('./constants');
    var Version = require('./version');
    var PrologEngine = require('./prolog_engine')
    var ReactWidget = require('./react_widget');
    var PrologState = require('./prolog_state');
    var ReactComponent = require('./react_component.js');
    console.log("Proactive v" + Version + " is ready");
    if (onProactiveReady !== undefined)
        onProactiveReady({render: function(url, rootElementId, container, settings)
                          {
                              var engine = new PrologEngine(url, rootElementId, settings.errorHandler, settings.messageHandler, function(status, error)
                                                            {
                                                                if (status)
                                                                {
                                                                    // Make the initial props
                                                                    var initialPropsObject = PrologState.emptyState;
                                                                    if (settings.propSpec != undefined && settings.propSpec != null)
                                                                    {
                                                                        var p = decodeURIComponent(settings.propSpec);
                                                                        initialPropsObject = new PrologState(Prolog._string_to_local_term(p));
                                                                    }
                                                                    var rootWidget = new ReactWidget(null, engine, rootElementId, initialPropsObject, function(widget)
                                                                                                     {
                                                                                                         container.className += " proactive_container vertical_layout vertical_fill horizontal_fill";
                                                                                                         container.appendChild(widget.getDOMNode());
                                                                                                         if (settings.callback != undefined)
                                                                                                             settings.callback(true, error);
                                                                                                     });
                                                                    engine.setRootWidget(rootWidget);
                                                                    window.addEventListener('resize', function()
                                                                                            {
                                                                                                window.requestAnimationFrame(function()
                                                                                                                             {
                                                                                                                                 rootWidget.reRender(function()
                                                                                                                                                     {
                                                                                                                                                     });
                                                                                                                             });
                                                                                            });

                                                                }
                                                                else if (settings.callback != undefined)
                                                                    settings.callback(false, error);
                                                            });
                          },
                          registerPredicate: PrologEngine.registerPredicate,
			  registerComponent: ProactiveComponentFactory.registerComponent,
			  ReactComponent: ReactComponent,
			  Constants: Constants,
                          make_atom: Prolog._make_atom,
                          make_functor: Prolog._make_functor,
                          makeInteger: Prolog._make_integer,
                          atom_chars: Prolog._atom_chars,
                          numeric_value: Prolog._numeric_value,
                          portray: Prolog._portray,
                          forEach: Prolog._forEach,
                          createHandler: Prolog._make_local,
                          releaseHandler: Prolog._free_local,
                          isAtom: Prolog._is_atom,
                          unify: Prolog._unify,
                          toCanonicalString: function(w)
                          {
                              var qOp = Prolog._create_options();
                              Prolog._set_option(qOp, Prolog._make_atom("quoted"), Prolog._make_atom("true"));
                              var s = Prolog._format_term(qOp, 1200, w);
                              Prolog._free_options(qOp);
                              return s;
                          },
                          isCompound: function(t, name, arity)
                          {
                              if (!Prolog._is_compound(t))
                                  return false;
                              if (name != Prolog._atom_chars(Prolog._term_functor_name(t)))
                                  return false;
                              if (arity != Prolog._term_functor_arity(t))
                                  return false;
                              return true;
                          },
                          hasFunctor: function(t, f)
                          {
                              if (!Prolog._is_compound(t))
                                  return false;
                              return Prolog._term_functor(t) == f;
                          },
                          functorOf: Prolog._term_functor,
                          argOf: Prolog._term_arg
                         }
			);
}
console.log("Waiting for Prolog...");
if (typeof window.proactivePrefixURL !== 'undefined')
    window.proscriptPrefixURL = window.proactivePrefixURL;
else
    window.proscriptPrefixURL = "/";
require('proscript');


module.exports = {};
