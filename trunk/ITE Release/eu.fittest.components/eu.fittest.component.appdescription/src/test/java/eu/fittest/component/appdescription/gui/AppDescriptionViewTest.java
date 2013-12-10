package eu.fittest.component.appdescription.gui;

import javax.swing.JFrame;

import eu.fittest.common.core.exception.FITTESTException;

public class AppDescriptionViewTest {

	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {
		AppDescriptionView view = new AppDescriptionView();
		view.setVisible(true);
		view.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

}
