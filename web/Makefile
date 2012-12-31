hakyll : hakyll.hs
	ghc --make hakyll

preview : hakyll
	./hakyll preview 8001

rebuild : hakyll
	./hakyll rebuild

publish : hakyll
	./hakyll rebuild
	rsync -tvr _site/* cis194@eniac.seas.upenn.edu:html/
	ssh cis194@eniac.seas.upenn.edu chmod -R o+rX html
