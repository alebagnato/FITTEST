# Location of flex-sdk: 		
FLEX=c:/apps/flex4.5.1
 
# Building the logger
all: 
	$(FLEX)/bin/mxmlc -library-path+=./flashloggerLib \
	-incremental=true \
	-debug=true \
	-source-path . \
	-output ./logger.swf  \
	-- ./MyLogger.as

clean:
	rm logger.swf	
  
