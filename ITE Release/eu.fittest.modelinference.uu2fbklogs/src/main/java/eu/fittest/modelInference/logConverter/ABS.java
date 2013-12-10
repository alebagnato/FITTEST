package eu.fittest.modelInference.logConverter;

import java.util.*;

/**
 * ABS definition 
 * 
 * @author Alessandro Marchetto
 *
 */
public class ABS {
	
	//http://www.fileformat.info/tool/regex.htm
	//http://www.regular-expressions.info/examples.html
	
	public String[] absStateChecker=null; // [0] regex , [1] if regex matches
	public Vector<String[]> vectOfCheckerXType=null; // sequences of pair_RegExp_Value
	public Hashtable<String,Vector<String[]>> absList; // type, v
	
	public ABS(){
		absList=new Hashtable<String,Vector<String[]>>();
		setABS();
		vectOfCheckerXType=null;
		absStateChecker=null;
	}
	
	public void setABS(){
		
		///////	 String (original) 
		
		vectOfCheckerXType=new Vector<String[]>();
		
		absStateChecker=new String[2];
		absStateChecker[0]=""; //regex  
		absStateChecker[1]="Empty"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]=".*"; //regex
		absStateChecker[1]="!Empty"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("string", vectOfCheckerXType);
		
		///////	 String (customized for flexcart) 
		
		vectOfCheckerXType=new Vector<String[]>();
		
		absStateChecker=new String[2];
		absStateChecker[0]="[\\$ ][0\\.]*"; //regex  
		absStateChecker[1]="Empty"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="[\\$]*.*"; //regex
		absStateChecker[1]="!Empty"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("string", vectOfCheckerXType);
		
		///////	int	
		
		vectOfCheckerXType=new Vector<String[]>();

		absStateChecker=new String[2];
		absStateChecker[0]="[0]*"; //regex
		absStateChecker[1]="Zero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="^\\d+$"; //regex
		absStateChecker[1]="GreaterThanZero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());

		absStateChecker=new String[2];
		absStateChecker[0]="^-\\d+$"; //regex
		absStateChecker[1]="LessThanZero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("int", vectOfCheckerXType);
			
		///////	Number	
		
		vectOfCheckerXType=new Vector<String[]>();
		
		absStateChecker=new String[2];
		absStateChecker[0]="^\\d*\\.{0,1}\\d+$"; //regex
		absStateChecker[1]="GreaterThanZero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="[0]*"; //regex
		absStateChecker[1]="Zero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="^-\\d*\\.{0,1}\\d+$"; //regex
		absStateChecker[1]="LessThanZero"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("number", vectOfCheckerXType);
		
		///////	Array	
		
		vectOfCheckerXType=new Vector<String[]>();
		
		absStateChecker=new String[2];
		absStateChecker[0]="( ; ){1}"; //regex
		absStateChecker[1]="Empty"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="((\\s*;.*[a-zA-Z0-9]*\\s*)){2,}"; //regex
		absStateChecker[1]="MoreThanOneItem"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		//absStateChecker[0]="( ;.*[a-zA-Z0-9]*){1}+"; //regex
		absStateChecker[0]="((\\s*;.*[a-zA-Z0-9]*\\s*)){1}+"; //regex
		absStateChecker[1]="OneItem"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("array", vectOfCheckerXType);
		
		///////	Boolean	
		
		vectOfCheckerXType=new Vector<String[]>();
			
		absStateChecker=new String[2];
		absStateChecker[0]="true"; //regex
		absStateChecker[1]="True"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absStateChecker=new String[2];
		absStateChecker[0]="false"; //regex
		absStateChecker[1]="False"; //if regex matches
		vectOfCheckerXType.add(absStateChecker.clone());
		
		absList.put("boolean", vectOfCheckerXType);
	
		///////
	
	}
	
}
