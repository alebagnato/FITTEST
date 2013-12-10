package eu.fittest.component.junit;

import java.io.File;

import junit.framework.Assert;

import org.junit.Test;


public class JUnitComponentTest {

	@Test
	public void loadResource() throws Exception{
		File f = new File("./artifacts/user-extensions.js");
		Assert.assertTrue(f.exists());
	}
}
