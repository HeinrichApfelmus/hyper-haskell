.PHONY: test run interpreter pkg-darwin

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

pkg-darwin:
	mkdir -p build && electron-packager app --out=build/ --platform=darwin --icon=resources/icons/icon.icns
