/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.flashdriver;

import junit.framework.Assert;

import com.thoughtworks.selenium.FlashSelenium;
import com.thoughtworks.selenium.Selenium;


public class FlashApplication extends FlashSelenium {
	private static final String AUTOMATION_LOADER = "AutomationLoader";

	public FlashApplication(Selenium selenium) 
	{
		super(selenium, AUTOMATION_LOADER);
	}
	
	public String invoke(String objectID, String method, String... args)
	{
		return invoke(5,1000, objectID, method, args);
	}
	public String invoke(int tries, int delay, String objectID, String method, String... args)
	{
		try 
		{
			Thread.sleep(delay);
		} catch (InterruptedException e) 
		{ // ignore interrupts
		}
		while(tries > 0)
		{
			tries--;
			String result = call("TestObject", objectID);
			if("true".equals(result) )
			{
				String[] arguments = new String[args.length+2];
				arguments[0] = objectID; 
				arguments[1] = method;
				System.arraycopy(args, 0, arguments, 2, args.length);
				return call("Invoke",  arguments);
			}
		}
		Assert.fail("Could not find object with id: "+objectID);
		return null;
	}
}
