PORT=10080


SRC= src/React.java                     \
     src/ReactDiff.java                 \
     src/ReactEdit.java                 \
     src/ReactComponent.java            \
     src/Panel.java                     \
     src/Button.java                    \
     src/Field.java                     \
     src/PatchSet.java                  \
     src/Title.java                     \
     src/Engine.java                    \
     src/PrologDocument.java            \
     src/PrologNode.java                \
     src/PrologElement.java             \
     src/PrologText.java                \
     src/Predicate_java_println.java    \
     src/PrologState.java               \
     src/PrologContext.java             \
     src/ReactComponentFactory.java     \
     src/RootPanel.java                 \
     src/ReactApp.java                  \
     src/ReactEditNode.java             \
     src/ReactEditProps.java            \
     src/ReactEditRemove.java           \
     src/ReactEditInsert.java           \
     src/ReactEditOrder.java            \
     src/ReactEditInsert.java           \
     src/ReactEditWidget.java           \
     src/ReactEditText.java             \
     src/CodeChangeListener.java        \
     src/ServerConnection.java          \
     src/PrologThunk.java               \
     src/ReactEditThunk.java            \
     src/Predicate_on_server.java       \
     src/ReactLeafComponent.java        \
     src/ReactEnvironment.java          \
     src/ReactLoaderState.java          \
     src/ReactModule.java               \
     src/Predicate_colon.java           \
     src/ReactUserModule.java

.src:	$(SRC)
	echo $(SRC) > $@


build:
	mkdir build

client:	.src build
	javac -cp gpj.jar:java_websocket.jar -Xlint:unchecked @.src -d build
	jar cvf react.jar -C build/ . 

run:	client
	java -cp gpj.jar:java_websocket.jar:react.jar React "http://localhost:${PORT}/react" "App"

run-server:
	swipl -f src/server.pl -g "start_react_server(${PORT})"
