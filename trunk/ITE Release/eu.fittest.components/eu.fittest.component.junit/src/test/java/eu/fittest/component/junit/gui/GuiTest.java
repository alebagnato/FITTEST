package eu.fittest.component.junit.gui;

import javax.swing.JFrame;

import eu.fittest.common.core.exception.FITTESTException;

public class GuiTest {

	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {
		JunitDescriptionView view = new JunitDescriptionView();
		view.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		view.open();
	}

}
