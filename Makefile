CC = clang
CC_FLAGS = -fPIC -shared
SHARED_LIB = $(CC) $(CC_FLAGS)
OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d
EXTNS = -XPackageImports

all: shared
	ghc $(OPTS) $(EXTNS) src/cbits.so src/prnt_d.so --make src/*.hs -o kaleidoscope

shared:
	$(SHARED_LIB) src/prnt_d.c -o src/prnt_d.so
	$(SHARED_LIB) src/cbits.c -o src/cbits.so

clean:
	rm src/*.so
	rm src/*.hi
	rm src/*.o
	rm kaleidoscope
