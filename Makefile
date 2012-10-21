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
	cp -r *.el haxe.texi Makefile README hxtags.sh yasnippets \
project-templates ac-haxe wisent custom ede TAGS ${PACKAGE}

prepare:
	mkdir -p ${PACKAGE}
	mkdir -p ${DOCDIR}

byte-compile:
	emacs -Q -L . -batch -f batch-byte-compile ./custom/*.el
	emacs -Q -L . -batch -f batch-byte-compile ./ede/*.el
	emacs -Q -L . -batch -f batch-byte-compile *.el

clean:
	rm -f *.elc
	rm -rf ${PACKAGE}

# I don't quite understand this...
install:
	emacs -Q -L . -batch -l etc/install ${DIR}

document: $(DOCS:.texi=.info)

tar.bz2: default
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: default
	zip -r ${PACKAGE}.zip ${PACKAGE}

package: tar.bz2 zip
