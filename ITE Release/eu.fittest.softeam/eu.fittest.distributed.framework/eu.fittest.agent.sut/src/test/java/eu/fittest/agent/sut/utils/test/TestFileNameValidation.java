package eu.fittest.agent.sut.utils.test;

import junit.framework.Assert;

import org.junit.Test;

import eu.fittest.common.util.Validation;


public class TestFileNameValidation {

	@Test
	public void testFileName(){
		Assert.assertEquals("eu.fittest.loggerLogger-1233_2343", Validation.formatToValidFileName("eu.fittest.logger::Logger-1233_2343"));
	}
}
