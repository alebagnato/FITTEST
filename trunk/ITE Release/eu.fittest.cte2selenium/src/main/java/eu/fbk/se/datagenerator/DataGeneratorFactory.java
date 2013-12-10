package eu.fbk.se.datagenerator;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;

public class DataGeneratorFactory {
	public static IDataGenerator createDataGenerator(ComplexDataSpecType dataClz){
		if ("double".equals(dataClz.getBase().getLocalPart())
				|| "decimal".equals(dataClz.getBase().getLocalPart())){
			return new DoubleGenerator();
		} 

		if ("integer".equals(dataClz.getBase().getLocalPart())
				|| "long".equals(dataClz.getBase().getLocalPart())
				|| "short".equals(dataClz.getBase().getLocalPart())
				|| "int".equals(dataClz.getBase().getLocalPart())){
			return new IntegerGenerator();
		} 

		if ("string".equals(dataClz.getBase().getLocalPart())){
			return new StringGenerator();
		} 
		
		if ("date".equals(dataClz.getBase().getLocalPart())){
			return new DateGenerator();
		}

		if ("boolean".equals(dataClz.getBase().getLocalPart())){
			return new BooleanGenerator();
		}
		
		return null;
	}
}
