GHC="../ghc"

all:
	$(GHC) -threaded Main.hs

clean:
	rm -f *.hi *.o Main
