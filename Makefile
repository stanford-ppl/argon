###.PHONY: spatial 
.PHONY: apps

all: lang

lang:
	sbt compile

clean:
	bin/clean-dsl.sh

