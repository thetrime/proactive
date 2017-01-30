# Note that to compile the js client you must have the following in your path:
#  Empscripten
#  npm
#  browserify
#  uglifyjs
# Note also that this makefile does NOT build proscript, gmpjs (yet) or the ios version (Probably ever)

PORT=10080
VERSION := $(shell cat VERSION)

include Swing.src


BOILERPLATE = src/boilerplate.pl                              \
	      src/vdiff.pl

CLIENTS?=swing-client js-client

all:	$(CLIENTS)

.src:	$(SWING_SRC) Swing.src
	echo $(SWING_SRC) > $@

build:
	mkdir build

.PHONY: $(CLIENTS) package

swing-client: proactive-${VERSION}/proactive.jar

js-client: proactive-${VERSION}/proactive.js proactive-${VERSION}/proscript.js.mem

ifeq ($(OS), Windows_NT)
CLASSPATH_SEPARATOR=\;
else
CLASSPATH_SEPARATOR=:
endif

proactive-${VERSION}/proactive.jar:	.src build $(BOILERPLATE) proactive-${VERSION}
	javac -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar -Xlint:unchecked @.src -d build
	jar cvf proactive-${VERSION}/lib/proactive.jar -C build/ . -C src boilerplate.pl -C src vdiff.pl

proactive-${VERSION}/proactive.js:	$(JS_SRC) proactive-${VERSION}
# We have to disable warnings here because emscripten generates output containing a HUGE amount of unused vars and functions
# and uglify produces pages and pages of warnings about them if we dont stop it
	browserify --standalone Proactive -t brfs src/core.js | uglifyjs -m -c warnings=false > proactive-${VERSION}/proactive.js

proactive-${VERSION}/proactive.css:	css/proactive.css
	cp $< $@

proactive-${VERSION}/proscript.js.mem:	node_modules/proscript/proscript.js.mem
	cp $< $@

run-swing-client:	swing-client
	java -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar${CLASSPATH_SEPARATOR}proactive-${VERSION}/lib/proactive.jar org.proactive.React "http://localhost:${PORT}/react" "App"

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT}), ['demo/App']"

proactive-${VERSION}:
	mkdir -p proactive-${VERSION}/lib proactive-${VERSION}/src
	cp src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl proactive-${VERSION}/src/

package: $(CLIENTS) src/jsx.pl src/vdiff.pl src/react.pl src/dom.pl
	zip -r proactive-${VERSION}.zip proactive-${VERSION}
#	rm -rf proactive-${VERSION}

clean:
	rm -rf build
	rm -rf proactive-*
	rm -f .src
