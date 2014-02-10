package eu.fittest.agent.sut.ui;

import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public abstract class AbstractAgentUI implements IAgentView{
	protected HUTEnvironment _env = null;
	protected HUTType _type = null;
	protected String _description = null; 
	
	protected AbstractAgentUI() {
		_description = System.getProperty("os.name")+", "+System.getProperty("os.version")+", "+System.getProperty("os.arch");
	}
	
	public HUTEnvironment getHUTEnvironment() {
		return _env;
	}

	public HUTType getHUTType() {
		return _type;
	}
	
	public String getDescription() {
		return _description;
	}
}
