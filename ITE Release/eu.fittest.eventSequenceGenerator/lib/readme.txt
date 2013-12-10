This artifact must be deployed in some maven repository (local or remote). E.g.:

*********Deploying to remote FITTEST Maven Repository*****************
Before trying to deploy the jar, the credentials for the fittest Maven repository must have been configured. Please check https://ddp.berner-mattner.com/svn/FITTEST/Software/SOFTEAM/release/readme.txt

mvn deploy:deploy-file -DpomFile=pom.xml -Dfile=new_IMU.0.0.2.jar -DrepositoryId=fittest-releases -Durl=https://webdav-staq.dsic.upv.es/nexus/content/repositories/releases

*********Deploying to the local Maven repository**********************

mvn install:install-file -Dfile=new_IMU.0.0.2.jar -DpomFile=pom.xml