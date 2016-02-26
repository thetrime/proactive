SRC= React.java ReactDiff.java ReactEdit.java ReactComponent.java Panel.java Button.java Field.java PatchSet.java Title.java Engine.java PrologDocument.java PrologNode.java PrologElement.java PrologText.java Predicate_java_println.java PrologState.java PrologContext.java ReactComponentFactory.java RootPanel.java ReactApp.java ReactEditNode.java ReactEditProps.java ReactEditRemove.java ReactEditInsert.java ReactEditOrder.java ReactEditInsert.java ReactEditWidget.java ReactEditText.java CodeChangeListener.java ServerConnection.java PrologThunk.java ReactEditThunk.java Predicate_on_server.java ReactLeafComponent.java

all:	$(SRC)
	javac -cp gpj.jar:java_websocket.jar -Xlint:unchecked $(SRC)

run:
	java -cp gpj.jar:java_websocket.jar:. React
