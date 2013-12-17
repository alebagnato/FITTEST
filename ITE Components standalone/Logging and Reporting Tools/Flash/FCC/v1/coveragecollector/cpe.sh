# Script by urueda@UPVLC (tested under MacOS)

# Universitat Politecnica de Valencia 2013
# Camino de Vera, s/n
# 46022 Valencia, Spain
# www.upv.es    

# author: Urko Rueda Molina
# version 1.0
# package coveragecollector

echo "Script (tested under MacOS) based on <awk, diff, sed and grep> to separate coverage points of Flexstore versions into:"
echo -e "\ta) same/equal source code points"
echo -e "\tb) modified/new functionality between source code versions points"
echo "--"
echo ""
cd flexstore_coverage

#<<COMMENT
echo "1/4) Will remove duplicated lines from coverage points (maven instrumentation seems to generate dup points)"
for flexstore in flexstore*
do
	echo -e "\t--- Removing duplicate coverage point lines for $flexstore ---"
	cd $flexstore
	awk '!x[$0]++' coveragePoints.txt > coveragePointsClean.txt
	cd ..
	echo -e "\t$flexstore done and saved to $flexstore/coveragePointsClean.txt"
done
echo ""
#COMMENT

#<<COMMENT
echo "2/4) Will get content of line numbers (coverage points) to .cpe files"
for flexstore in flexstore*
do
	echo -e "\t--- Getting line contents for $flexstore ---"
	cd $flexstore
	rm -f $flexstore.cpe
	cat coveragePointsClean.txt | while read cpline
	do
		file=`echo $cpline | awk '{print $1}' | sed -E 's/;+/\//g'`
		ln=`echo $cpline | awk '{print $2}'`
		echo $ln\ ^_@_^ $file >> $flexstore.cpe
		sed -n $ln\p $file >> $flexstore.cpe
		echo "" >> $flexstore.cpe
	done
	cd ..
	echo -e "\tsaved to $flexstore/$flexstore.cpe"
done
echo ""
#COMMENT

# next is only applicable to multiple SUT versions (e.g. five flexstore versions)
<<COMMENT
echo "3/4) Will identify modified/new functionality between flexstore versions"
for fv in 2 3 4 5
do
	if [ $fv -eq 5 ]; # flexstore5?
	then
		fvr=3 # compare against flexstore3
	else
	fvr=$(( fv - 1 ))
	fi
	echo -e "\t--- Diff report on flexstore$fv over flexstore$fvr ---"
	reportfile="flexstore$fv/cpediff.txt"
	cpleft="" # (left part of diff)
	cpd="" # cpright (right part of diff)
	rm -f $reportfile
	diff -i -E -b -w -B --left-column --suppress-common-lines --minimal flexstore$fvr/flexstore$fvr.cpe flexstore$fv/flexstore$fv.cpe | while read diffline
	do
		if [ "$cpleft" == "" ];
		then
			#echo "${diffline}" | egrep "< [0-9]+ \^_@_\^"
			result=$(echo "${diffline}" | egrep "< [0-9]+ \^_@_\^") # no console output
			#if [ "${?}" -eq 0 ];
			if [ -n "$result" ];
			then
				#echo "CPLEFT GOT"
				cpleft=$diffline
			fi
		elif [ "$cpd" == "" ];
		then
			#echo "${diffline}" | egrep "> [0-9]+ \^_@_\^"
			result=$(echo "${diffline}" | egrep "> [0-9]+ \^_@_\^") # no console output
			#if [ "${?}" -eq 0 ];
			if [ -n "$result" ];
			then
				# check if cpleft and cpright relate to same source file
				cplfile=`echo $cpleft | awk '{print $4}' | sed "s/flexstore$fvr/flexstore$fv/g"`
				cprfile=`echo $diffline | awk '{print $4}' | sed "s/flexstore$fvr/flexstore$fv/g"`
				if [ "$cplfile" == "$cprfile" ];
				then
					#echo "CPRIGHT GOT"
					cpd=$diffline
				else
					cpleft="" # skip non matching source files comparison
				fi
			fi
		else # check if there is content from diff
			echo "${diffline}" | egrep "[0-9]+[a-d][0-9]+" # a new diff entry?
			if [ "${?}" -ne 0 ];
			then
				echo "DIFFERENCE GOT"
				echo $cpd >> $reportfile # flexstore$fv differs
			fi
			cpd=""	
		fi
	done
	echo -e "\treport saved to $reportfile"
done
echo ""
COMMENT

# next is only applicable with multiple SUT versions (e.g. five flexstore versions)
<<COMMENT
echo "4/4) Will mark functionality (coverage points) that differs between flexstore versions"
for fv in 2 3 4 5
do
	if [ $fv -eq 5 ]; # flexstore5?
	then
		fvr=3 # compare against flexstore3
	else
	fvr=$(( fv - 1 ))
	fi
	reportfile="cpediff.txt"
	echo -e "--- Mark coverage points that differs on flexstore$fv over flexstore$fvr ---"
	cd flexstore$fv
	cpefile="coveragePointsExtended.txt"
	rm -f $cpefile
	cat coveragePointsClean.txt | while read cpline
	do
		differ="false"
		rawfile=`echo $cpline | awk '{print $1}'`
		file=`echo $rawfile | sed -E 's/;+/\//g'`
		ln=`echo $cpline | awk '{print $2}'`
		if [ -a $reportfile ]; # if file exists there are differences between versions
		then
			#egrep " $ln \^_@_\^ $file" cpediff.txt
			result=$(egrep " $ln \^_@_\^ $file" cpediff.txt) # no console output
			#if [ "${?}" -eq 0 ];
			if [ -n "$result" ];
			then
				differ="true"
			fi
		fi
		if [ $differ == "false" ];
		then
			# check if source file is new (no diff is detected for such case)
			rawfiler=`echo $rawfile | sed "s/flexstore$fv/flexstore$fvr/g"`
			#egrep "$rawfiler" ../flexstore$fvr/coveragePointsClean.txt
			result=$(egrep "$rawfiler" ../flexstore$fvr/coveragePointsClean.txt) # no console output
			#if [ "${?}" -ne 0 ];
			if [ -z "$result" ];
			then
				differ="true"
			fi
		fi
		if [ $differ == "true" ];
		then
			echo $cpline _DIFFER_ >> $cpefile		
		else
			echo $cpline >> $cpefile		
		fi
	done
	echo -e "\tmarking done for flexstore$fv to file: flexstore$fv/$cpefile"
	cd ..
done
echo ""
COMMENT