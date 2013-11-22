package fbk.se.mutation.xpath;

import java.util.ArrayList;
import java.util.List;

public class VerySimpleXpathParser {
	private String xpathExpr;
	private String[] xpathElements;
	
	/**
	 * Result a list of elements of the path
	 * 
	 * Assum that xpath has the following simple format: [path]/propertyName
	 *  - last element is the property name where we want to access or change value
	 *  - path = /(element/)+
	 * 
	 * @return
	 */
	public List<String> getFullPath(){
		if (xpathExpr != null){
			if (xpathElements.length > 0){
				List<String> ret = new ArrayList<String>();
				for (int i = 1; i < xpathElements.length; i++){
					ret.add(xpathElements[i]);
				}
				return ret;
			}
		}
		
		return null;
	}

	/**
	 * Assum that xpath has the following simple format: [path]/propertyName
	 * return only the [path] part.
	 * 
	 */
	public List<String> getPrincipalPath(){
		if (xpathExpr != null){
			if (xpathElements.length > 0){
				List<String> ret = new ArrayList<String>();
				for (int i = 1; i < xpathElements.length - 1; i++){
					ret.add(xpathElements[i]);
				}
				return ret;
			}
		}
		
		return null;
	}
	
	/**
	 * Return only the "path" path of the fullpath, exclude the property name  [path]/propertyName
	 * @return
	 */
	public String rebuildPrincipalPath(){
		if (xpathExpr != null){
			if (xpathElements.length > 0){
				String ret = "";
				for (int i = 1; i < xpathElements.length - 1; i++){
					ret = ret + "/" + xpathElements[i];
				}
				return ret;
			}
		}
		
		return null;
	}
	
	
	/**
	 * Get the last element of xpathExpr assuming that it's the target field
	 * @return
	 */
	public String getTargetFieldName(){
		if (xpathExpr!= null){
			if (xpathElements.length > 0){
				return xpathElements[xpathElements.length-1];
			}
		}
		return null;
	}


	/**
	 * The only Constructor 
	 * @param xpathExpr
	 */
	public VerySimpleXpathParser(String xpathExpr) {
		this.xpathExpr = xpathExpr;
		if (xpathExpr != null){
			xpathElements = xpathExpr.split("/");
		}
	}

}
