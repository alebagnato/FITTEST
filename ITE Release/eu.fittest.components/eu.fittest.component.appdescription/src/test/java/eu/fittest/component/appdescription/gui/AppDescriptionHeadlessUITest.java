package eu.fittest.component.appdescription.gui;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.component.appdescription.headless.AppDescriptionHeadlessUI;

public class AppDescriptionHeadlessUITest {

	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {
		AppDescriptionHeadlessUI view = new AppDescriptionHeadlessUI();
		view.open();
	}

}
