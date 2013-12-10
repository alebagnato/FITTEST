package eu.fittest.common.core.identity.spec;

public class FITTESTComponent extends FITTESTEntity{
	
	private FITTESTAgent containerAgent;

	public FITTESTComponent(String id) {
		super(id);
	}

	public FITTESTComponent() {
		this(null);
	}

	public FITTESTAgent getContainerAgent() {
		return containerAgent;
	}

	public void setContainerAgent(FITTESTAgent containerAgent) {
		this.containerAgent = containerAgent;
	}
	
}
