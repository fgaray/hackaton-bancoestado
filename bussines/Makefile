# USE swipl-ld -v -shared -o file file.c to get the flags
PROLOG=-lswipl

# USE /usr/bin/python2.7-config --ldflags
# To get the flags if this is not working
PYTHON=-L/usr/lib/python2.7/config-x86_64-linux-gnu -L/usr/lib -lpython2.7 -lpthread -ldl  -lutil -lm


all: test
	stack build --ghc-options="$(PROLOG) $(PYTHON)"

repl:
	stack ghci --ghci-options='-fobject-code -O0 $(PROLOG) $(PYTHON)'

clean:
	stack clean

run:
	stack exec bussines-exe

.PHONY: test
test:
	stack test --ghc-options="$(PROLOG) $(PYTHON)"
