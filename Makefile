output:=scripts/elm.js	
sources:=Main.elm
srcdir:=src

all: $(output)

$(output): $(sources:%=$(srcdir)/%)
	elm make --output=$@ $^

clean:
	rm $(output)
	rm -r elm-stuff

.PHONY: all clean
