#!/usr/bin/python

import sys
import re

usage = "parseLog.py infile outfile"

lineCounter = 0

numOfSelectedItems = 0
numInShopCart = 0
cartTotal = ''
numInCompareCart = 0
catalogContents = ''

writeHeaders = True

if (len(sys.argv) != 3):
	print usage
else:
	inFile = open(sys.argv[1], 'r')
	outFile = open(sys.argv[2], 'w')
	lines = inFile.readlines()
	checkLines = False
	stateInfo = False
	eventName = ''
	
	for line in lines:
		if line.strip().startswith('%<S "O:AppAbstractState"'):
			checkLines = True
			stateInfo = True
			lineCounter = 0
		elif line.strip().startswith('%<S "O:eu.fittest.actionscript.automation::RecordEvent"'):
			checkLines = True
			stateInfo = False
			lineCounter = 0
		elif line.strip() == '%>':
			checkLines = False
		if checkLines and lineCounter > 2:
			if stateInfo:
				_line = line.strip('%<{ ').rstrip(' }%>\n')
				items = _line.split('=')
				varName = items[0]
				val = items[1].split(':')[0]
				
				if varName == 'numOfSelectedItems':
					numOfSelectedItems = val
				elif varName == 'numInShopCart':
					numInShopCart = val
				elif varName == 'cartTotal':
					#cartTotal = re.sub('(%<{ cartTotal="\$)|(":([A-Za-z]+) }%>)|[\r\n]+','',line)
					cartTotal = val.replace("$","").replace(",", "").strip('"');
				elif numInCompareCart == 'numInCompareCart':
					numInCompareCart = val
				elif catalogContents == 'catalogContents':
					catalogContents = val
			else:
				eventName = re.sub('(%<{ targetID=")|(":([A-Za-z]+) }%>)|[\r\n]+','',line)
				if eventName.startswith("img_") or eventName.startswith("details_") or eventName.startswith("purchase_") or eventName.startswith("compare_") or eventName.startswith("remove_shoppingCart_") or eventName.startswith("remove_compareCart_") or eventName.startswith("quantity_"):
						eventName = eventName.split('_')[0]
				checkLines = False
				
#			outFile.write(line.strip('%<{ ').rstrip(' }%>\n') + '\n')

		if lineCounter == 7 and stateInfo:
			if writeHeaders:
				outFile.write("EVENT,numOfSelectedItems,numInShopCart,cartTotal,numInCompareCart,catalogContents\n")
				writeHeaders = False
				
			outFile.write(eventName + ',' + str(numOfSelectedItems) + ',' + str(numInShopCart) + ',' + cartTotal + ',' + str(numInCompareCart) + ',"' + catalogContents + '"\n')
			
		lineCounter = lineCounter + 1
			
	inFile.close()
	outFile.close()
