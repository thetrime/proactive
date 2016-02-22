SRC= React.java ReactDiff.java ReactEdit.java ReactComponent.java Panel.java Button.java Field.java PatchSet.java Title.java

all:	$(SRC)
	javac $(SRC)

run:
	java React base.xml new.xml
