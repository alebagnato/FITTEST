package eu.fittest.eclipse.gui.wizards;

import org.eclipse.osgi.util.NLS;

public class FITTESTWizardMessages extends NLS {
	private static final String BUNDLE_NAME = "eu.fittest.eclipse.gui.wizards.messages"; 
	public static String FITTESTProject_NewTestingProjectWizardName;
	public static String FITTESTProjectWizardPageOne_PageName;
	public static String FITTESTRecordingSessionWizard_Name;
	public static String FITTESTReplayingSessionWizard_Name;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, FITTESTWizardMessages.class);
	}

	private FITTESTWizardMessages() {
	}
}
