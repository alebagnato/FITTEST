package eu.fittest.agent.sut.ui.gui;

import javax.swing.JFrame;

import eu.fittest.common.core.exception.FITTESTException;

public class TestGUI {

	/**
	 * @param args
	 * @throws FITTESTException 
	 */
	public static void main(String[] args) throws FITTESTException {
		JFrame frame = new SUTAgentFrame();
		frame.setVisible(true);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

}
