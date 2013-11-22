#!/bin/awk -f
{ 
	if (index($0, "->") > 0) {
		lastIndex = index($4, "\"]")
		label = substr($4,9,lastIndex-9)
		print $1" "$2" ["label"] "$3";"
	}
}