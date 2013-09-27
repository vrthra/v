

v : V.hs VPrim.hs VData.hs Main.hs
	ghc --make -o $@ $^ -package parsec
	-rm *.hi *.o

test: V.hs Test.hs
	ghc --make -o $@ $^ 
	-rm *.hi *.o
