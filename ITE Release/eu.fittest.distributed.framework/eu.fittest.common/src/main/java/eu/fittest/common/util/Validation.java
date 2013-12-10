package eu.fittest.common.util;

public class Validation {
    public static String formatToValidFileName(String s){
    	String charToReplace ="[^a-zA-Z\\.0-9\\-_]"; 
		return s.replaceAll(charToReplace, "");
    }
}
