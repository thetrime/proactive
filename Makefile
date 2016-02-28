PORT=10080

SRC= src/React.java                                        \
     src/ReactComponent.java                               \
     src/ReactComponentFactory.java                        \
     src/CodeChangeListener.java                           \
     src/ServerConnection.java                             \
     src/ReactLeafComponent.java                           \
     src/ReactComponentFactoryConfiguration.java           \
     src/ui/Panel.java                                     \
     src/ui/Button.java                                    \
     src/ui/Field.java                                     \
     src/ui/Title.java                                     \
     src/ui/RootPanel.java                                 \
     src/ui/ReactApp.java                                  \
     src/ui/DefaultReactComponentFactoryConfiguration.java \
     src/prolog/Engine.java                                \
     src/prolog/PrologState.java                           \
     src/prolog/PrologContext.java                         \
     src/prolog/ReactEnvironment.java                      \
     src/prolog/ReactLoaderState.java                      \
     src/prolog/ReactModule.java                           \
     src/prolog/ReactUserModule.java                       \
     src/prolog/FluxDispatcher.java                        \
     src/prolog/FluxEventListener.java                     \
     src/prolog/Predicate_java_println.java                \
     src/prolog/Predicate_on_server.java                   \
     src/prolog/Predicate_colon.java                       \
     src/prolog/Predicate_raise_event.java                 \
     src/prolog/Predicate_wait_for.java                    \
     src/vdom/PatchSet.java                                \
     src/vdom/ReactDiff.java                               \
     src/vdom/ReactEdit.java                               \
     src/vdom/PrologDocument.java                          \
     src/vdom/PrologNode.java                              \
     src/vdom/PrologElement.java                           \
     src/vdom/PrologText.java                              \
     src/vdom/PrologThunk.java                             \
     src/vdom/ReactEditNode.java                           \
     src/vdom/ReactEditProps.java                          \
     src/vdom/ReactEditRemove.java                         \
     src/vdom/ReactEditInsert.java                         \
     src/vdom/ReactEditOrder.java                          \
     src/vdom/ReactEditInsert.java                         \
     src/vdom/ReactEditWidget.java                         \
     src/vdom/ReactEditText.java                           \
     src/vdom/ReactEditThunk.java

all:	client

.src:	$(SRC)
	echo $(SRC) > $@

build:
	mkdir build

.PHONY: client

client: dist/react.jar

dist/react.jar:	.src build
	javac -cp dist/gpj.jar:dist/java_websocket.jar -Xlint:unchecked @.src -d build
	jar cvf dist/react.jar -C build/ . -C src boilerplate.pl

run-client:	client
	java -cp dist/gpj.jar:dist/java_websocket.jar:dist/react.jar org.proactive.React "http://localhost:${PORT}/react" "App"

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT})"

clean:
	rm -rf build
	rm dist/react.jar
:.*
