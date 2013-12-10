package eu.fittest.eclipse.gui.utils;

import javax.naming.InvalidNameException;

public class Validation {
	static public boolean containsBadChar(String text) throws InvalidNameException{	
		String charToReplace ="[A-Z]| |_|[a-z]|[0-9]|\\."; 
		if(text.replaceAll(charToReplace, "").length()>0){
			throw new InvalidNameException(charToReplace);
		}
		else return false;
	}
}
