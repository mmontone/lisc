lisc: 
	ghc -fignore-asserts -O -o lisc --make Lisc.hs

clean:
	-rm lisc 
	-rm -r *.hi *.o *~ 
	-rm -r doc 


doc:    
	-mkdir doc 
	haddock -h -o doc *.hs
