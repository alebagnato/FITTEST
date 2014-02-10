for rqs in thirdpartytools dependencies
do
	echo "About to install $rqs ..."
	cd $rqs
	./installLocallyAll.sh
	cd ..
	echo ""
done