package eu.fittest.agent.sut.services.component.spec;

public class ComponentData {
	private String _componentId;
	private String _agentId;
	private String _iteId;
	private String _componentDir;
	
	public void setFittestComponentId(String componentId) {
		_componentId = componentId;
	}

	public void setFittestAgentId(String agentId) {
		_agentId = agentId;		
	}

	public void setFittestIteId(String iteId) {
		_iteId = iteId;		
	}

	public String getFittestComponentId() {
		return _componentId;
	}

	public String getFittestAgentId() {
		return _agentId;
	}
	
	public String getFittestIteId() {
		return _iteId;
	}

	public void setFittestComponentDir(String componentDir) {
		_componentDir = componentDir;
		
	}
	
	public String getFittestComponentDir() {
		return _componentDir;		
	}
}
