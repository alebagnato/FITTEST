echo ""
echo "Script by urko.rueda@UPVLC"
echo "Enables or disables the macos fix"
echo "--"
echo ""

if [ $1 == "enable" ] ;
then
	find . -name "*.macosfix" -exec script/enable_macos_fix_for.sh {} \;
elif [ $1 == "disable" ] ; 
then
	find . -name "*.orig" -exec script/disable_macos_fix_for.sh {} \;
else
	echo "Uses:"
	echo -e "\tmacos_fix enable ... (enables the fix)"
	echo -e "\tmacos_fix disable ... (disables the fix)"
fi