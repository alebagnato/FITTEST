package eu.fittest.modelInference.logConverter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Vector;

/**
 * It detects the abstract states. 
 * It takes a pair: field type - field value;
 * 
 * @author Alessandro Marchetto
 *
 */
public class ABSstateDetection {
	ABS abs=new ABS();

	String absCheckerAndSelection(String ty, String value) {
		try{
		String type=ty.trim().toLowerCase();
		if (abs.absList.get(type)==null) return null;
		
		Vector<String[]> vectOfCheckerXType=abs.absList.get(type);
		
		Pattern patt;
		Matcher matcher;
		
		for (String[] absState : vectOfCheckerXType) {
			//System.out.print("'"+absState[0]+"' vs '"+value+"'");
			 
			patt=Pattern.compile(absState[0],Pattern.CASE_INSENSITIVE | Pattern.CANON_EQ | Pattern.UNICODE_CASE);
			matcher = patt.matcher(value);
			
			if(matcher.matches()){
				//System.out.print(" ->yes \n");
				return absState[1];
			}
			//else  System.out.print(" \n");
		}
		
		return null;
		
		}catch(Exception e){
			e.printStackTrace();
			return null;
			}
		} 
	
	
}
