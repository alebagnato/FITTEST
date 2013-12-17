# Script by urueda@UPVLC (tested under MacOS)

# Universitat Politecnica de Valencia 2013
# Camino de Vera, s/n
# 46022 Valencia, Spain
# www.upv.es    

# author: Urko Rueda Molina
# version 1.0
# package coveragecollector

echo "Script (tested under MacOS) based on <diff, sed, grep> to compare the raw coverage information between user and testcase coverages:"
echo -e "\ta) Percentage of code exercised for each raw coverage report"
echo -e "\tb) Differing code exercise between raw coverage reports"
echo "Call format: cvgdiff pivotCvgRawfile.rpw compareCvgRawfile.rpw"
echo "Call example: ./cvgdiff.sh flexstore_coverage/flexstore1/coveragePointsClean_userCoverage.rpw flexstore_coverage/flexstore1/coveragePointsClean_testcaseCoverage.rpw"
echo "--"
echo ""

# both .rpw files should contain the same coverage points
es=$(grep "_E_" $1 | wc -l)
ds=$(grep "_D_" $1 | wc -l)
ts=$(( $es + $ds ))

dffA=()
if [ $es -gt 0 ];
then
	dffA=(_E_)
fi
if [ $ds -gt 0 ];
then
	dffA=($dffA _D_)
fi

for isdff in ${dffA[*]}
do
	if [ $isdff == "_E_" ];
	then
		p=$(bc -l <<< "$es * 100 / $ts")
		echo "EQUAL points = $es out of $ts ($(printf "%.5s\n" "$p")%)"
		echo "------------"
	elif [ $isdff == "_D_" ];
	then
		p=$(bc -l <<< "$ds * 100 / $ts")
		echo "DIFFERING points = $ds out of $ts ($(printf "%.5s\n" "$p")%)"
		echo "----------------"
	fi
	echo ""
	
	for iscvg in EXERCISED NOT_EXERCISED
	do

		echo -e "\t$iscvg CODE:"
		for rawcvg in $1 $2
		do
			ec=$(grep "= $iscvg" $rawcvg | grep $isdff | wc -l)
			lc=$(cat $rawcvg | grep $isdff | wc -l | sed "s/ //g")
			p=$(bc -l <<< "$ec * 100 / $lc")
			echo -e "\t$ec out of $lc ($(printf "%.5s\n" "$p")%) for $rawcvg"
		done
		echo ""

		echo -e "\t\t$iscvg DIFFERENCE over $1"
		ed2=$(diff -i -E -b -w -B --left-column --suppress-common-lines --minimal $1 $2 | egrep ">" | grep " $iscvg" | grep $isdff | wc -l)
		ec2=$(grep "= $iscvg" $2 | grep $isdff | wc -l | sed "s/ //g")
		p=$(bc -l <<< "$ed2 * 100 / $ec2")
		echo -e "\t\t$ed2 out of $ec2 ($(printf "%.5s\n" "$p")%) $iscvg DIFFERENCE for $2; RELATED CODE LINES:"

		diff -i -E -b -w -B --left-column --suppress-common-lines --minimal $1 $2 | egrep ">" | grep " $iscvg" | grep $isdff | while read diffline
		do
			echo -e "\t\t\t${diffline}"
		done
		echo ""
	
	done
	
done