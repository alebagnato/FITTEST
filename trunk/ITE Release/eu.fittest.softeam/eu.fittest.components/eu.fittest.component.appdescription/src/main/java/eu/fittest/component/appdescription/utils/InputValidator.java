package eu.fittest.component.appdescription.utils;

import java.io.File;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

public class InputValidator {
	static public boolean checkURL(String url){
		try {
			URLConnection connection = new URL(url).openConnection();
			connection.connect();
			return connection instanceof HttpURLConnection && ((HttpURLConnection)connection).getResponseCode() == HttpURLConnection.HTTP_OK;
		} catch (Exception e) {
			return false;
		}		
	}
	
	static public  boolean checkExecutableJar(String path){
		return new ExecutableJarFilter().accept(new File(path));
	}
}
