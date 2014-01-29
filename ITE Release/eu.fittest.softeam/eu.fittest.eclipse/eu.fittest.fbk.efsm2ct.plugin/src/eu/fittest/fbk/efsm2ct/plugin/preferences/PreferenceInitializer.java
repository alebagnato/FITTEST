package eu.fittest.fbk.efsm2ct.plugin.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import eu.fittest.fbk.efsm2ct.plugin.Activator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();

		if (System.getProperty("user.name").equals("tiella")) {

			store.setDefault(PreferenceConstants.P_PATH,
					System.getProperty("user.home")
							+ "/programmi/txl10.6.linux64");

		} else {

			store.setDefault(PreferenceConstants.P_PATH,
					"/usr/local/txl10.6.linux64");

		}

	}

}
