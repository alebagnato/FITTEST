package eu.fittest.common.core.identity.spec;

import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class FITTESTAgent extends FITTESTEntity{
	private HUTEnvironment _environment;
	private HUTType _type;
	private String _description;
	
	public String getDescription() {
		return _description;
	}

	public HUTEnvironment getEnvironment() {
		return _environment;
	}

	public HUTType getType() {
		return _type;
	}

	public FITTESTAgent() {
		this(null,null,null,null);
	}
	
	public FITTESTAgent(String id) {
		this(id,null,null,null);
	}
	
	public FITTESTAgent(HUTEnvironment environment, HUTType type, String description) {
		this(null,environment,type,description);
	}
	
	public FITTESTAgent(String id, HUTEnvironment environment, HUTType type, String description) {
		super(id);	
		_environment = environment;
		_type = type;
		_description = description;
	}

}
