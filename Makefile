run: 
	cd solutions/Day${day} && ghc Main.hs && ./Main ${part}

clean:
	find . -name "*.hi" -or -name "*.o" -or -name "Main" | xargs rm
