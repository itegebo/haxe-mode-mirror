# So far only builds the documentation for haxe-mode (what's available)
# and bytecode compilation of *.el files

PACKAGE = haxe-mode
DOCDIR = ${PACKAGE}/docs
BIN = $(PWD)/bin
IC = makeinfo
ICO = --force
DOCS = ./haxe.texi

.SUFFIXES: .texi .info

.texi.info:
	$(IC) $(ICO) -o $(DOCDIR)/$*.info $*.texi

default: prepare byte-compile document
	cp -r *.el Makefile README hxtags.sh yasnippets project-templates ac-haxe ${PACKAGE}

prepare:
	mkdir -p ${PACKAGE}
	mkdir -p ${DOCDIR}

byte-compile:
	emacs -Q -L . -batch -f batch-byte-compile *.el

clean:
	rm -f *.elc
	rm -rf ${PACKAGE}

# I don't quite understand this...
install:
	emacs -Q -L . -batch -l etc/install ${DIR}

document: $(DOCS:.texi=.info)

tar.bz2: package
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: package
	zip -r ${PACKAGE}.zip ${PACKAGE}
