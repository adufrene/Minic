FILES=Mini.java


Mini.class : antlr.generated ${FILES}
	javac *.java

antlr.generated: antlr.generated.mini antlr.generated.json
	touch antlr.generated

antlr.generated.mini : Mini.g
	java org.antlr.Tool Mini.g
	touch antlr.generated.mini

antlr.generated.json : ToJSON.g
	java org.antlr.Tool ToJSON.g
	touch antlr.generated.json

clean:
	\rm -f *generated* MiniParser.java MiniLexer.java ToJSON.java Mini.tokens ToJSON.tokens *.class
