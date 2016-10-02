.PHONY: test run interpreter

ELECTRON=/Applications/Electron.app/Contents/MacOS/Electron

STACK=stack --stack-yaml=haskell/stack.yaml

test: interpreter
	TESTING=1 $(ELECTRON) app
	killall hyper-haskell-server

run: interpreter
	$(ELECTRON) app

#hyper:
#	cd haskell && stack exec ghci -- -isrc src/Hyper.hs

interpreter:
	$(STACK) build
