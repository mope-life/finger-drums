output:=scripts/elm.js
srcdir:=src

# is this a normal way to write a Makefile?

sources:=$(patsubst %,$(srcdir)/%, \
    AudioModuleMap.elm             \
)

# these include .elm files that don't actually produce a Program
dependencies:=$(patsubst %,$(srcdir)/%, \
    AudioModule.elm                     \
    AudioModule/Control.elm             \
)

# also depend on this Makefile
.EXTRA_PREREQS+=$(lastword $(MAKEFILE_LIST))
.RECIPEPREFIX=|
.PHONY: all clean

all: $(output)

$(output): $(sources) $(dependencies)
#/
| elm make --output=$@ $(sources)
#\

clean:
#/
| rm $(output)
| rm -r elm-stuff
#\
