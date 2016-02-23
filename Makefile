SRC= React.java ReactDiff.java ReactEdit.java ReactComponent.java Panel.java Button.java Field.java PatchSet.java Title.java Engine.java PrologDocument.java PrologNode.java PrologElement.java PrologText.java Predicate_java_println.java PrologState.java PrologContext.java

all:	$(SRC)
	javac -cp gpj.jar $(SRC)

run:
	java -cp gpj.jar:. React
