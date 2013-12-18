# Location of flex-sdk: 		
# specify the path to your local installation of flex SDK 
# Change to the folder where you instlled SDK.
ifndef OSTYPE
  OSTYPE = $(shell uname -s | awk '{print tolower($$0)}')
  #export OSTYPE
endif

ifeq ($(OSTYPE), linux-gnu)
  FLEX=/home/alex/PROJECTS/FITTEST/Software/tools/flex
else ifeq ($(OSTYPE), darwin)
  FLEX="/Users/cdnguyen/programs/flex_sdk_4.6"
  #FLEX="/Applications/Adobe Flash Builder 4.5/sdks/4.5.0"
else
  #FLEX=c:/apps/flex4.0
  #FLEX=c:/apps/flex4.1
  FLEX=c:/apps/flex4.5.1
endif
 
# The compilers:
MXMLC=$(FLEX)/bin/mxmlc
COMPC=$(FLEX)/bin/compc

# Location of the AutomationFramework source base:
AUTOMATIONLIB=./automationframework

# turn on to compile with debug-mode enabled
DEBUG=-debug=true

# turn on to enable incremental compilation
INCRmode=-incremental=true

# Build the FITTEST automation-lib, and wrap it as swc:
automation.swc:
	echo "*** building automation.swc"
	$(COMPC) -source-path $(AUTOMATIONLIB)/src \
	-output ./bin/automation.swc \
	$(INCRmode) \
	$(DEBUG) \
	-include-classes eu.fittest.actionscript.automation.Automation  eu.fittest.actionscript.automation.Delegate eu.fittest.actionscript.automation.ClickableDelegate eu.fittest.actionscript.automation.Command eu.fittest.actionscript.automation.RecordEvent eu.fittest.actionscript.automation.InitEvent 
	
# Build the automation-loader:	
# -static-link-runtime-shared-libraries=true 
AutomationLoader.swf: automation.swc	
	echo "*** Bulding AutomationLoader.swf"
	$(MXMLC) -library-path+=./bin/automation.swc \
	$(INCRmode) \
	$(DEBUG) \
	-source-path $(AUTOMATIONLIB)/src \
	-output ./bin/AutomationLoader.swf  \
	-- $(AUTOMATIONLIB)/src/eu/fittest/actionscript/automation/loader/AutomationLoader.as

# Build the set of flex automation-delegates:
FlexDelegates.swf: automation.swc 
	echo "*** Building set of flex automation-delegates"
	$(MXMLC) -library-path+=./bin/automation.swc \
	$(INCRmode) \
	$(DEBUG) \
	-source-path $(AUTOMATIONLIB)/src \
	-static-link-runtime-shared-libraries=true \
	-output ./bin/FlexDelegates.swf  \
	-- $(AUTOMATIONLIB)/src/eu/fittest/actionscript/automation/delegates/flex/FlexDelegates.as
	
flashloggerLib.swc: automation.swc
	$(COMPC) -library-path+=./bin/automation.swc \
	-source-path ./src \
	$(DEBUG) \
	$(INCRmode) \
	-output ./bin/flashloggerLib.swc \
	-include-classes eu.fittest.Logging.Serialization.Delegates eu.fittest.Logging.Serialization.Serializable eu.fittest.Logging.Serialization.Serializer eu.fittest.Logging.Serialization.FittestSerializer eu.fittest.Logging.Serialization.SimpleSerializer eu.fittest.Logging.Serialization.SomeSerializationDelegates eu.fittest.Logging.Serialization.EventSerializableDelegates eu.fittest.Logging.Serialization.MxUISerializableDelegates eu.fittest.Logging.LAppEvent eu.fittest.Logging.DynAppState eu.fittest.Logging.LoggerBase eu.fittest.Logging.ByteArrayLogger eu.fittest.Logging.FittestLogger eu.fittest.Logging.replay.LogReplay eu.fittest.Logging.FittestLoggerHook
  

all: automation.swc AutomationLoader.swf FlexDelegates.swf FlexDelegates.swf flashloggerLib.swc

# to copy the components into the example:
copy:
	cp ./bin/*.swc ./example/flashloggerLib/
	cp ./bin/*.swf ./example/
	
# to build asdocs:
asdocs:
	$(FLEX)/bin/asdoc -source-path ./src ./automationframework/src -doc-sources ./src ./automationframework/src -output ./asdocs
clean:
	rm -r -f ./bin/*
