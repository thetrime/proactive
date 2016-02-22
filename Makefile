SRC= React.java ReactDiff.java ReactEdit.java ReactComponent.java 

all:	$(SRC)
	javac $(SRC)

run:
	java React base.xml new.xml
