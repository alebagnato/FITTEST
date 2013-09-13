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
