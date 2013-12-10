package eu.fittest.agent.sut;

import java.awt.Frame;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JOptionPane;

import eu.fittest.agent.sut.ui.controller.AgentGUIController;
import eu.fittest.agent.sut.ui.gui.SUTAgentFrame;
import eu.fittest.agent.sut.ui.headless.SUTAgentDefault4Logging;
import eu.fittest.agent.sut.ui.headless.SUTAgentHeadlessUI;
import eu.fittest.agent.sut.ui.headless.SUTAgentNoUI;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;

public class Main {

	/**
	 * Main method
	 * 
	 * @param args
	 * @throws FITTESTException
	 */
	public static void main(String[] args) throws FITTESTException {
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_START,
				FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_START
						.toString());
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_RANGE,
				FITTESTConstants.DEFAULT_FITTEST_ITE_SERVER_PORT_RANGE
						.toString());
		System.setProperty(FITTESTConstants.FITTEST_SERVER_PORT_INCREMENT,
				FITTESTConstants.DEFAULT_FITTEST_SERVER_PORT_INCREMENT
						.toString());
		System.setProperty(
				FITTESTConstants.FITTEST_SERVICE_FILETRANSFER_BASEDIR,
				FITTESTConstants.DEFAULT_FITTEST_SERVICE_FILETRANSFER_BASEDIR);
		System.setProperty(
				FITTESTConstants.FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY,
				Long.toString(FITTESTConstants.DEFAULT_AUTOREGISTRATION_DELAY));

		String uiEnableValue = System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_UI_ENABLED, "false");

		if ("false".equals(uiEnableValue) && !AppLock.setLock("fittest.agent.SUTAgent")) {
			JOptionPane
					.showMessageDialog(new Frame(),
							"There is a FITTEST agent running on your system, please close it first!");
			System.exit(0);
		}

//		if ("false".equals(uiEnableValue)) {
//			try {
//				if (new File(new URI(SUTAgentNoUI.CONF_FILE)).exists()) {
//					final AgentGUIController agentController = new AgentGUIController(
//							new SUTAgentNoUI());
//					addHookAndOpen(agentController);
//				} else {
//					final AgentGUIController agentController = new AgentGUIController(
//							new SUTAgentDefault4Logging());
//					addHookAndOpen(agentController);
//				}
//			} catch (URISyntaxException e) {
//				final AgentGUIController agentController = new AgentGUIController(
//						new SUTAgentHeadlessUI());
//				addHookAndOpen(agentController);
//			}
//		} else {
			final AgentGUIController agentController = new AgentGUIController(
					new SUTAgentFrame());
			addHookAndOpen(agentController);
//		}

	}

	/**
	 * Add exit hook and start
	 * 
	 * @param agentController
	 */
	private static void addHookAndOpen(final AgentGUIController agentController) {
		if (agentController == null)
			return;
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				if (agentController != null && agentController.isRegistered()) {
					agentController.deregister();
				}
			}
		});
		agentController.open();
	}
}
