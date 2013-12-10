package eu.fbk.xinputmining.logdata;

import java.util.ArrayList;
import java.util.List;

import com.stromberglabs.cluster.UniPoint;

public abstract class LogData {
	public static final String DATA_TYPE_INT = "int";
	public static final String DATA_TYPE_FLOAT = "float";
	public static final String DATA_TYPE_STRING = "string";
	public static final String[] ALL_DATA_TYPES = {DATA_TYPE_STRING, DATA_TYPE_INT, DATA_TYPE_FLOAT};
	
	public static final int MIN_CLUSTERING_SIZE = 10;
	
	
	@SuppressWarnings("rawtypes")
	List entries;
	String dataType;
	
	@SuppressWarnings("unchecked")
	public <T> List<T> getEntries(Class<T> type) {
		 List<T> result = new ArrayList<T>();
		 for(Object e : entries) {
			if (type.isAssignableFrom(e.getClass())) {
				result.add((T)e);
			}
		 }
		 return result;
	}

	
	@SuppressWarnings("rawtypes")
	public List getEntries() {
		return entries;
	}


	@SuppressWarnings("rawtypes")
	public LogData() {
		entries = new ArrayList();
	}
	
	/**
	 * Should apply clustering or not
	 * @return
	 */
	public boolean shouldBeClustered(){
		if (entries.size() > MIN_CLUSTERING_SIZE 
				&& !dataType.equals(DATA_TYPE_STRING)){
			return true;
		} else {
			return false;
		}
	}
	
	public boolean shouldBeEnumerated(){
		if (entries.size() <= MIN_CLUSTERING_SIZE)
			return true;
		return false;
	}
	
	public boolean shouldUseStringBoundary(){
		if (entries.size() >= MIN_CLUSTERING_SIZE 
				&& dataType.equals(DATA_TYPE_STRING))
			return true;
		return false;		
	}
	
	public abstract void add(String value);

	public String getDataType() {
		return dataType;
	}

	public String format(Object value){
		if (dataType.equals(DATA_TYPE_STRING))
			return value.toString();
		if (dataType.equals(DATA_TYPE_INT) && value instanceof UniPoint){
			int intVal = (int)((UniPoint) value).getValue(); 
			return String.valueOf(intVal); 
		}
		if (dataType.equals(DATA_TYPE_STRING))
			return value.toString();
		return "";
	}
	
}
