MAINCLASS = Main

all: build

run:
	java -Xms256m -Xmx512m -cp src ${MAINCLASS}
	
pack:
	zip hw2.zip -r Makefile src

clean:
	rm -rf out
	rm -f src/*.java

build:
	javac -cp src src/Main.java
	
out:
	mkdir -p out