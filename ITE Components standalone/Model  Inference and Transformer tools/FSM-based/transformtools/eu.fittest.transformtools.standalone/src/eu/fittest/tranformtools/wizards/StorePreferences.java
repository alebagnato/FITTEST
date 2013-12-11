/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.wizards;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.osgi.service.prefs.BackingStoreException;


import eu.fittest.tranformtools.Activator;

public class StorePreferences {
	
	// Only works in Eclipse 3.7
	//private static IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(Activator.PLUGIN_PREFERENCE_SCOPE);
	
	@SuppressWarnings("deprecation")
	private static InstanceScope scope = new InstanceScope();
	private static IEclipsePreferences prefs = scope.getNode(Activator.PLUGIN_PREFERENCE_SCOPE);
	
	public static String getSavedValue(String key){
		return prefs.get(key, "");  
	}
	
	public static void saveValue(String key, String value){
		prefs.put(key, value);
		try {
			prefs.flush();
		} catch (BackingStoreException e) {
			// skip
		}
	}
}
