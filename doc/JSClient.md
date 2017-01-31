Proactive/JS
============

Using the JS client, you can render directly to HTML. This means you can make web pages out of Proactive code. In theory, anything that renders should be supported in the JS client, but in practise there may be differences in layout and capability between the clients.

Proactive/JS uses Proscript as its Prolog engine, available as a submodule in the proactive repository.

If you use swipl as your web server, and proactive is available at /assets/proactivejs/ then you can serve up a Proactive module Module by defining an HTTP handler like this:
```
serve_form(Request):-
        memberchk(path(Path), Request),
        <Obtain Module somehow, eg from the Path>
        subtract(Request, [path(_)], R1),
        parse_url(URL, [path('/react')|R1]),
        format(atom(RenderCommand),'                   Proactive.render("~w", ~w, "~w", document.getElementById("container"), function(status, error) {if (!status && error == 403) location.reload();}, displayErrorDialog);', [URL, Props, FormId]),
        atomic_list_concat(['window.proactivePrefixURL="/assets/proactivejs/";',
                            'console.log("Configured mem prefix as /assets/proactivejs/");',
                            'onProactiveReady = function(Proactive)',
                            '{',
                            '      displayErrorDialog = function(ex)',
                            '      {',
                            '           <Handle exceptions here>;',
                            '      };',
                            '      SeaChart(Proactive);',
                            '      Proactive.registerPredicate("open_url", function(w) { window.open(Proactive.atom_chars(w));});',
                            RenderCommand,
                            '}'], '\n', Bootstrap),

        HTML = element(html, [], [element(head, [], [element(link, [rel=stylesheet, type='text/css', href='/assets/proactivejs/proactive.css'], [])]),
                                  element(body, [], [element(div, [id=container, style='width: 100%;'], []),
                                                     element(script, [type='text/javascript'], [Bootstrap]),
                                                     element(script, [type='text/javascript', src='/assets/proactivejs/proactive.js'], [])])]),
        format(current_output, 'Content-type: text/html~n~n', []),
        html_write(current_output, HTML, []).
```

Building
--------
To build proactivejs is a bit of a chore. The dependencies are:
   * uglifyjs and browserify on your path
   * proscript, which requires:
      * emscripten on your path
      * gmp-js, which requires:
         * emscripten on your path

If you have all this, you can just run
```
make CLIENTS="js-client" package
```