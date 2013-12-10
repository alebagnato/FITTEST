ASC=~/Work/flex_sdk_4/bin/mxmlc
ASIC=../compiler/dist/build/asic/asic
FILE=game.swf

all:
	$(ASC) -compiler.source-path=. -optimize -static-link-runtime-shared-libraries code/MyGame.as -o $(FILE)
	$(ASIC) --dump-ast $(FILE) --verbose > game-ast.txt
	$(ASIC) --env=../lib/src/InstrBaseLib.hs --gen-lib=GameSym $(FILE) --verbose -o GameSym.hs
