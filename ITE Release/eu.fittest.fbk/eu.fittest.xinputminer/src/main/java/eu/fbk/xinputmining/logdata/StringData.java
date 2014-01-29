package eu.fbk.xinputmining.logdata;

public class StringData extends LogData {

	public StringData() {
		super();
		dataType = DATA_TYPE_STRING;
	}
	
	@Override
	public void add(String value) {
		if (!getEntries().contains(value))
			getEntries().add(value);
	}
	
	public int getMaxLength(){
		int maxLen = Integer.MIN_VALUE;
		for (Object obj : getEntries()){
			if (maxLen < obj.toString().length()){
				maxLen = obj.toString().length();
			}
		}
		return maxLen;
	}

	public int getMinLength(){
		int minLen = Integer.MAX_VALUE;
		for (Object obj : getEntries()){
			if (minLen > obj.toString().length()){
				minLen = obj.toString().length();
			}
		}
		
		return minLen;
		
	}
}
