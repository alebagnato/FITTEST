package eu.fittest.eclipse.oraclesuite.utils;

import org.eclipse.core.expressions.PropertyTester;

public class SingleFolderCommandScopeTester extends PropertyTester {

	public SingleFolderCommandScopeTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		
		return true;	}

}
