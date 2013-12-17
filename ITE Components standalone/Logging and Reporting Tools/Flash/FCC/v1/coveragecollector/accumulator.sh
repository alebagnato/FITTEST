# Script by urueda@UPVLC (tested under MacOS)

# Universitat Politecnica de Valencia 2013
# Camino de Vera, s/n
# 46022 Valencia, Spain
# www.upv.es    

# author: Urko Rueda Molina
# version 1.0
# package coveragecollector

echo ""
echo "Script (tested under MacOS) based on <find, awk, grep, sed> to accumulate individual coverage reports"
echo "All reports are expected to be equal length (same source files lines)"
echo "There are 3 columns for each source file:"
echo -e "\t1) Line number that is a) EXERCISED, b) PARTIAL exercised or c) NOT exercised"
echo -e "\t2) % of reports covering each source line"
echo -e "\t3) % covered of the source file lines by all reports (ignore all but last)"
echo ""
rawFile="raw.acc"
echo "Note: all .rpw reports in the current (sub)dir will be accumulated and a raw report will be saved to $rawFile"
echo "--"
echo ""

src_piv="-1" # init value
src_lines=1
exc=0
_Eexec=0
_TEexec=0
_Dexec=0
_TDexec=0

pvt_rep=$(find . -name *.rpw | head -1)
echo "PIVOT = $pvt_rep"
rn=$(find . -name *.rpw | wc -l) # reports number

echo -n "Reports number = $rn"

cat $pvt_rep |
{ # command grouping to run all in the same shell

	while read line
	do
	
		src=$(echo $line | awk '{print $1}')
		
		if [ $src != $src_piv ];
		then
			echo ""
			echo ""
			echo "$src"
			src_piv=$src
			src_lines=1
			exc=0
		fi
		
		ln=$(echo $line | awk '{print $3}')
		isExc=$(echo $line | awk '{print $5}')
		n=$(grep -H -r "$line" . --include=*.rpw | wc -l | sed 's/ //g') # n is > 0

		echo -n "$src @ $ln = " >> $rawFile
		
		if [[ $src_piv == _E_* ]];
		then
			_TEexec=$(( $_TEexec + 1 ))
		elif [[ $src_piv == _D_* ]];
		then
			_TDexec=$(( $_TDexec + 1 ))
		fi
		
		if [ $isExc == "EXERCISED" ] ;
		then
			cvg=$n
			exc=$(( $exc + 1 ))
			if [[ $src_piv == _E_* ]];
			then
				_Eexec=$(( $_Eexec + 1 ))
			elif [[ $src_piv == _D_* ]];
			then
				_Dexec=$(( $_Dexec + 1 ))		
			fi
			echo -n "Line < $ln > is EXERCISED"
			echo "EXERCISED" >> $rawFile
		elif [ $isExc == "NOT_EXERCISED" ];
		then
			cvg=$(( $rn - $n ))
			if [ $(( $rn - $n )) -gt 0 ] ;
			then
				exc=$(( $exc + 1 ))
				if [[ $src_piv == _E_* ]];
				then				
					_Eexec=$(( $_Eexec + 1 ))
				elif [[ $src_piv == _D_* ]];
				then
					_Dexec=$(( $_Dexec + 1 ))		
				fi			
				#echo -n "Line < $ln > PARTIAL exercised"
				echo -n "Line < $ln > is EXERCISED" # whether it is partial or not is seen in the coverage %
				echo "EXERCISED" >> $rawFile
			else
				echo -n "Line < $ln > NOT exercised"
				echo "NOT_EXERCISED" >> $rawFile
			fi
		fi		
	
		p=$(bc -l <<< "$cvg * 100 / $rn")
		echo -e -n "\t\tcvg=[ $(printf "%.5s\n" "$p")% ] "
		#echo -e -n "\tsource lines = $src_lines"
		p=$(bc -l <<< "$exc * 100 / $src_lines")
		echo -e "\t\tsource exercised = $exc out of $src_lines lines $(printf "%.5s\n" "$p")%"
	
		src_lines=$(( $src_lines + 1 ))

	done

	echo ""

	# we still have access to global variables thanks to command grouping
	total=$(( _TEexec + _TDexec ))
	EDexec=$(( $_Eexec + $_Dexec ))
	p=$(bc -l <<< "$EDexec * 100 / $total")
	echo "Total points exercised: $EDexec out of $total ($(printf "%.5s\n" "$p")%)"
	if [ $_TEexec -gt 0 ];
	then
		p=$(bc -l <<< "$_Eexec * 100 / $_TEexec")
		echo -e "\tEqual points exercised: $_Eexec out of $_TEexec ($(printf "%.5s\n" "$p")%)"
	fi
	
	if [ $_TDexec -gt 0 ];
	then
		p=$(bc -l <<< "$_Dexec * 100 / $_TDexec")
		echo -e "\tDiffering points exercised: $_Dexec out of $_TDexec ($(printf "%.5s\n" "$p")%)"
	fi
	
	if [ $_TEexec -gt 0 ] && [ $_TDexec -gt 0 ];
	then
		echo ""
		echo "Stats:"
		p=$(bc -l <<< "$_TEexec * 100 / $total")
		echo -e "\tEqual points = $_TEexec out of $total ($(printf "%.5s\n" "$p")%)"
		p=$(bc -l <<< "$_TDexec * 100 / $total")
		echo -e "\tDiffering points = $_TDexec out of $total ($(printf "%.5s\n" "$p")%)"
	fi
}

echo ""