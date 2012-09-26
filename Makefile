# So far only builds the documentation for haxe-mode (what's available)

BIN = $(PWD)/bin
IC = makeinfo
ICO = --force
DOCS = ./haxe.texi

.SUFFIXES: .texi .info
.texi.info:
	$(IC) $(ICO) -o $(BIN)/$*.info $*.texi

default: document

clean:
	$(RM) -r $(BIN)/*

document: $(DOCS:.texi=.info)