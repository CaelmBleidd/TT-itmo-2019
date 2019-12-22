HC=ghc
SOURCES=src/Main.hs src/Grammar.hs
GEN_SOURCES=src/Lexer.x src/Parser.y
GENERATED=src/Lexer.hs src/Parser.hs
PACKAGE=HW2.zip

.PHONY: pack all run clean

all: parser

clean:
		rm -rf src/*.o src/*.hi
		rm -f parser

run: parser
		./parser

parser: $(GENERATED) $(SOURCES)
	$(HC) -O2 -i./src -tmpdir . ./src/Main.hs -o parser

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src
