package eu.fittest.agent.sut.services.component.spec;

import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.core.service.ServiceEvent;

public class ComponentRegistrationEvent extends ServiceEvent{
	private ComponentRegistrationEventKind _kind;
	private ComponentData _data;
	
	public ComponentRegistrationEvent(ILocalService source, ComponentData data, ComponentRegistrationEventKind kind) {
		super(source);
		_kind = kind;
		_data = data;
	}
	
	public ComponentRegistrationEventKind getKind(){
		return _kind;
	}
	
	public ComponentData getData(){
		return _data;
	}

}
