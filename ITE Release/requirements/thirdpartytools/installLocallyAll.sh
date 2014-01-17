for tool in dk.brics.automaton nl.flotsam.xeger netx flashselenium daikon haslog evosuite-fittest nz.ac.waikato.cms.weka cluster-weka
do
	echo "About to install $tool ..."
	cd $tool
	./installLocally.sh
	cd ..
	echo ""
done