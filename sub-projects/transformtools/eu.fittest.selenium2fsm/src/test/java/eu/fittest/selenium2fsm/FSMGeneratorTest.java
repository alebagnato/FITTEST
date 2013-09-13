package eu.fittest.selenium2fsm;


import java.io.File;

import org.junit.Test;

import eu.fittest.converter.LogConverterException;

public class FSMGeneratorTest {
	
	String projectRoot = System.getProperty("user.dir");
	
	@Test
	public void testModelGeneration() {
		String seleniumFolder = projectRoot 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "resources"
				+ File.separator + "selenium";
		
		String outputFolder = projectRoot 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "resources"
				+ File.separator + "output";
		
		FSMGenerator generator = new FSMGenerator();
		try {
			generator.generateFSM(seleniumFolder, outputFolder, "CuteNews");
		} catch (LogConverterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
