:-module(server,
	 [debug_message/1,
	  start_react_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:-ensure_loaded(user:testing).
:-ensure_loaded(user:react).
:-ensure_loaded(user:jsx).
:-ensure_loaded(user:vdiff).
:-ensure_loaded(user:dom).

user:term_expansion(end_of_file, _) :-
        prolog_load_context(module, Module),
        trigger_react_recompile(Module),
        fail.

start_react_server(Port):-
        http_server(http_dispatch, [port(Port)]).

:-multifile(react:goal_is_safe/1).
react:goal_is_safe(_).

debug_message(X):- writeln(X).

:-http_handler(root('proactive/'), serve_form, [prefix]).

serve_form(Request):-
        memberchk(path_info(FormId), Request),
        subtract(Request, [path(_)], R1),
        parse_url(URL, [path('/react')|R1]),
        format(atom(RenderCommand),'                   Proactive.render("~w", "~w", document.getElementById("container"), {errorHandler: function(status, error) {if (!status && error == 403) location.reload();}, function(ex) {console.log(ex);}, callback: function() { console.log("Initial rendering complete");}});', [URL, FormId]),
        atomic_list_concat(['window.proactivePrefixURL="/assets/lib/";',
                            'onProactiveReady = function(Proactive)',
                            '{',
                            RenderCommand,
                            '}'], '\n', Bootstrap),


        Body = element(body, [], [element(div, [id=container, style='width: 100%;'], []),
                                  element(script, [type='text/javascript'], [Bootstrap]),
                                  element(script, [type='text/javascript', src='/assets/lib/proactive.js'], [])
                                 ]),
        HTML = element(html, [], [element(head, [], [element(link, [rel=stylesheet, type='text/css', href='/assets/css/proactive.css'], []),
                                                     element(meta, [name='apple-mobile-web-app-capable', content=yes], []),
                                                     element(meta, [name='apple-mobile-web-app-title', content=FormId], []),
                                                     element(meta, [name='apple-mobile-web-app-status-bar-style', content=black], []),
                                                     element(meta, ['http-equiv'='cache-control', content='private, max-age=0, no-cache'], []),
                                                     element(meta, ['http-equiv'=pragma, content='no-cache'], []),
                                                     element(meta, ['http-equiv'='expires', content='0'], [])]),
                                  Body]),
        format(current_output, 'Content-type: text/html~n~n', []),
        html_write(current_output, HTML, []).


user:term_expansion(:-serve_assets, :-http_handler(root('assets/'), http_reply_from_files(Location, []), [prefix])):-
        setup_call_cleanup(open('VERSION', read, S),
                           read_string(S, _, Version),
                           close(S)),
        format(atom(Location), 'proactive-~s', [Version]).

:-serve_assets.
