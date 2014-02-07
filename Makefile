CC = clang
OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d
EXTNS = -XPackageImports

all:
	$(CC) -fPIC -shared src/cbits.c -o src/cbits.so
	ghc $(OPTS) $(EXTNS) src/cbits.so --make src/*.hs -o kaleidoscope

clean:
	rm src/cbits.so
	rm src/*.hi
	rm src/*.o
	rm kaleidoscope
