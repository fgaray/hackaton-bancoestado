PROLOG=/usr/lib/libswipl.a


all:
	stack build --ghc-options="$(PROLOG)"

repl:
	stack ghci --ghci-options='-fobject-code -O0 $(PROLOG)'

run:
	stack exec bussines-exe
