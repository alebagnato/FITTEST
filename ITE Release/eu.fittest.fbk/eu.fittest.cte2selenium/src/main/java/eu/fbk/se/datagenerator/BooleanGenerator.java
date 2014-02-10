package eu.fbk.se.datagenerator;

import java.util.Random;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.utils.Constants;

public class BooleanGenerator implements IDataGenerator {
	private static Random ranGenerator = new Random();

	public String generate(ComplexDataSpecType dataClz) {
		return String.valueOf(ranGenerator.nextBoolean());
	}

}
