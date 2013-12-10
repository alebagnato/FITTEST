package eu.fittest.agent.sut.ui;

import java.awt.event.ActionListener;
import java.util.logging.Level;

import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public interface IAgentView extends IServiceListener{
	HUTEnvironment getHUTEnvironment();
	HUTType getHUTType();
	String getDescription();
	void open();
	void close();
	void addExitActionListener(ActionListener l);
	void addRegisterActionListener(ActionListener l);
	void displayMessage(Level level, String message);
	void exit();
}
