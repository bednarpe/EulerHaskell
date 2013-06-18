clean:
	rm -rf *.o *.hi
clear: clean

%:
	@ghc p$@
	@./p$@