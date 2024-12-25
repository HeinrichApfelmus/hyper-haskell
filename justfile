ELECTRON := "electron"

VERSION := "0.2.3.0"

######################################################################
# Development targets

test:
	just build
	TESTING=1 {{ELECTRON}} app
	killall hyper-haskell-server

run:
	just build
	{{ELECTRON}} app

build:
	cabal build all

######################################################################
# Release targets

APP_NAME := "HyperHaskell"

DIR_DARWIN := "build/" + APP_NAME + "-darwin-arm64"
DIR_WIN32 := "build\\" + APP_NAME + "-win32-x64"

# You need to call
#
#   npm install --save-dev @electron/packager
#   npm install electron --save-dev
#
# before the following recipe has a chance of working
pkg-darwin:
	mkdir -p build \
		&& npx @electron/packager \
			app \
			--out=build \
			--overwrite \
			--platform=darwin \
			--arch=arm64 \
			--icon=resources/icons/icon.icns \
			--extend-info=resources/macOS-Info.plist \
		&& rm {{DIR_DARWIN}}/LICENSE \
		&& cp resources/LICENSE.electron.txt {{DIR_DARWIN}}/LICENSE.electron.txt \
		&& rm {{DIR_DARWIN}}/version

pkg-win32:
	cmd /C "(if not exist build md build) && (electron-packager app --out=build\ --overwrite --platform=win32 --icon=resources\icons\icon.ico) && (del $(DIR_WIN32)\LICENSE) && (echo F|xcopy resources\LICENSE.electron.txt $(DIR_WIN32)\LICENSE.electron.txt) && (del $(DIR_WIN32)\version)"

zip-darwin:
	cd {{DIR_DARWIN}} && zip -r ../HyperHaskell-v{{VERSION}}-darwin-x64.zip *

zip-win32:
	cmd /C "(cd {{DIR_WIN32}} && (7z a -tzip ..\HyperHaskell-v{{VERSION}}-win32-x64.zip .\)"

hackage:
	cabal sdist \
	&& cabal upload haskell/hyper \
	&& cabal upload haskell/hyper-extra \
	&& cabal upload haskell/hyper-haskell-server
