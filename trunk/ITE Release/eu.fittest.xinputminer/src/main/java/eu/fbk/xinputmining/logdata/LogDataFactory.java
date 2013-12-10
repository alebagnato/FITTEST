package eu.fbk.xinputmining.logdata;

import eu.fbk.xinputmining.MiningException;

public class LogDataFactory {
	public static LogData createLogData(String type) throws MiningException{
		if (type.equals(LogData.DATA_TYPE_STRING)){
			return new StringData();
		} else if (type.equals(LogData.DATA_TYPE_INT)){
			return new NumericData(LogData.DATA_TYPE_INT);
		} else if (type.equals(LogData.DATA_TYPE_FLOAT)){
			return new NumericData(LogData.DATA_TYPE_FLOAT);
		} else {
			throw new MiningException("Data type: " + type + " is not supported!");
		}
	}
}
