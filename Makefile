# Note that to compile the js client you must have the following in your path:
#  Empscripten
#  npm
#  browserify
#  uglifyjs

PORT=10080
VERSION := $(shell cat VERSION)

include Swing.src


BOILERPLATE = src/boilerplate.pl                              \
	      src/vdiff.pl

CLIENTS?=swing-client js-client

all:	$(CLIENTS)

.src:	$(SWING_SRC) Makefile
	echo $(SWING_SRC) > $@

build:
	mkdir build

.PHONY: $(CLIENTS) package

swing-client: dist/proactive.jar

js-client: dist/proactive.js dist/proscript.js.mem

ifeq ($(OS), Windows_NT)
CLASSPATH_SEPARATOR=\;
else
CLASSPATH_SEPARATOR=:
endif

dist/proactive.jar:	.src build $(BOILERPLATE)
	javac -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar -Xlint:unchecked @.src -d build
	jar cvf dist/proactive.jar -C build/ . -C src boilerplate.pl -C src vdiff.pl

dist/proactive.js:	$(JS_SRC)
# We have to disable warnings here because emscripten generates output containing a HUGE amount of unused vars and functions
# and uglify produces pages and pages of warnings about them if we dont stop it
	browserify --standalone Proactive -t brfs src/core.js | uglifyjs -m -c warnings=false > proactive-${VERSION}/proactive.js

dist/proactive.css:	css/proactive.css
	cp $< $@

dist/proscript.js.mem:	node_modules/proscript/proscript.js.mem
	cp $< $@

run-swing-client:	swing-client
	java -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar${CLASSPATH_SEPARATOR}dist/proactive.jar org.proactive.React "http://localhost:${PORT}/react" "App"

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT}), ['demo/App']"

package: $(CLIENTS) src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl
	mkdir -p proactive-${VERSION}/lib proactive-${VERSION}/src
	cp dist/proactive.jar proactive-${VERSION}/lib/proactive-${VERSION}.jar
	cp src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl proactive-${VERSION}/src/
	zip -r proactive-${VERSION}.zip proactive-${VERSION}
#	rm -rf proactive-${VERSION}

clean:
	rm -rf build
	rm -f dist/proactive.jar
	rm -f dist/proactive.js
	rm -f dist/proscript.js.mem
	rm -f .src
