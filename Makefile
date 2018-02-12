# Note that to compile the js client you must have the following in your path:
#  Empscripten
#  npm
#  browserify
#  uglifyjs
# Note also that this makefile does NOT build proscript, gmpjs (yet) or the ios version (Probably ever)

PORT?=10080
VERSION := $(shell cat VERSION)

include Swing.src
include JS.src


CLIENTS?=swing-client js-client
MINIFY?=true
TEST_APP?=App
REACT_SRC=proactive-${VERSION}/src/jsx.pl                     \
	  proactive-${VERSION}/src/vdiff.pl                   \
	  proactive-${VERSION}/src/react.pl                   \
	  proactive-${VERSION}/src/dom.pl

all:	$(CLIENTS) $(REACT_SRC)

.src:	$(SWING_SRC) Swing.src
	@echo $(SWING_SRC) > $@

build:
	mkdir build

.PHONY: $(CLIENTS) package

swing-client: proactive-${VERSION}/lib/proactive.jar

js-client: force-proscript-build proactive-${VERSION}/lib/proactive.js proactive-${VERSION}/css/proactive.css

ifeq ($(OS), Windows_NT)
CLASSPATH_SEPARATOR=\;
else
CLASSPATH_SEPARATOR=:
endif

proactive-${VERSION}/lib/proactive.jar:	.src build $(BOILERPLATE_GPJ)
	@mkdir -p proactive-${VERSION}/lib
	javac -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar -Xlint:unchecked @.src -d build
	jar cf proactive-${VERSION}/lib/proactive.jar -C build/ . -C src boilerplate.pl -C src boilerplate_gpj.pl -C src vdiff.pl

node_modules/brfs:
	npm install brfs

node_modules/xmlhttprequest:
	npm install xmlhttprequest

node_modules/uglifyjs:
	npm install uglifyjs

node_modules/browserify:
	npm install browserify

src/version.js: VERSION
	echo "module.exports='$(VERSION)';" > $@

src/Version.java: VERSION
	echo 'package org.proactive; public class Version { public static String version="$(VERSION)";}' > $@


proactive-${VERSION}/lib/proactive.js:	$(JS_SRC) $(REACT_SRC) node_modules/brfs node_modules/xmlhttprequest node_modules/uglifyjs node_modules/browserify
# We have to disable warnings here because emscripten generates output containing a HUGE amount of unused vars and functions
# and uglify produces pages and pages of warnings about them if we dont stop it
	mkdir -p proactive-${VERSION}/lib
ifeq ($(MINIFY),true)
	node_modules/browserify/bin/cmd.js --standalone Proactive -t brfs src/core.js | node_modules/uglifyjs/bin/uglifyjs  -m -c warnings=false > proactive-${VERSION}/lib/proactive.js
else
	node_modules/browserify/bin/cmd.js --standalone Proactive -t brfs src/core.js  > proactive-${VERSION}/lib/proactive.js
endif

proactive-${VERSION}/css/proactive.css:	css/proactive.css
	@mkdir -p proactive-${VERSION}/css
	cp $< $@

force-proscript-build:
	@make -C node_modules/proscript


run-swing-client:	swing-client
	java -cp dist/gpj.jar${CLASSPATH_SEPARATOR}dist/java_websocket.jar${CLASSPATH_SEPARATOR}proactive-${VERSION}/lib/proactive.jar org.proactive.React ${SWING_ARGS} "http://localhost:${PORT}/react" ${TEST_APP}

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT}), ['demo/App']"

proactive-${VERSION}/src/jsx.pl: src/jsx.pl
	@mkdir -p  proactive-${VERSION}/src
	cp $< $@

proactive-${VERSION}/src/vdiff.pl: src/vdiff.pl
	@mkdir -p  proactive-${VERSION}/src
	cp $< $@

proactive-${VERSION}/src/react.pl: src/react.pl
	@mkdir -p  proactive-${VERSION}/src
	cp $< $@

proactive-${VERSION}/src/dom.pl: src/dom.pl
	@mkdir -p  proactive-${VERSION}/src
	cp $< $@

package: force-proscript-build $(CLIENTS) $(REACT_SRC)
	mkdir -p proactive-${VERSION}
	zip -r proactive-${VERSION}.zip proactive-${VERSION}
#	rm -rf proactive-${VERSION}

clean:
	make -C node_modules/proscript clean
	rm -rf build
	rm -rf proactive-*
	rm -f .src
