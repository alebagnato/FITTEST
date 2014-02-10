for tool in haslog daikon lopi dclmerge dtracesplit iteagent_1.0.1 IMU xinputminer_1.0.0 xinputminer_1.0.1 fbkflex_1.0.0
do
	echo "About to install $tool ..."
	cd $tool
	./installLocally.sh
	cd ..
	echo ""
done

echo "Pending: manually copy phpmaven dependency to your local repository"