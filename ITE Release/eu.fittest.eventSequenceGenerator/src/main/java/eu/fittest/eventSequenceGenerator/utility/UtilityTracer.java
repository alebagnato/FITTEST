package eu.fittest.eventSequenceGenerator.utility;

/**
*
* @author Alessandro
*
*/
public class UtilityTracer {

	 public static String convert(String[] stateVaraibles){
		  String str="";
		  for (int i = 0; i < stateVaraibles.length; i++) {
			  if (i==0) {
				  if (!stateVaraibles[i].equals("")) 
					  str=stateVaraibles[i];
			  }
			  else {
				  if ((!stateVaraibles[i].equals(""))&&(!str.equals(""))) 
					  str=str+";__;"+stateVaraibles[i];
				  else if ((!stateVaraibles[i].equals(""))&&(str.equals("")))
					  str=stateVaraibles[i];
			  }
			
		}
		  return str;
	  }
	
}
