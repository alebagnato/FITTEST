ASC=~/Work/flex_sdk_4/bin/mxmlc
FILE=temp.swf

all:
	# Compile
	$(ASC) -compiler.source-path=. -optimize -static-link-runtime-shared-libraries SerialExample.as -o $(FILE)

	# Run through asic
	../compiler/dist/build/asic/asic temp.swf -o temp-bla.swf --dump-ast > ast.txt
