package eu.fittest.agent.sut.services.serialization.spec;

import eu.fittest.common.core.service.ServiceEvent;

public class SerializationEvent extends ServiceEvent<ILocalSerializationService>{
	private String _resource;
	private SerializationEventKind _kind;
	
	public SerializationEvent(ILocalSerializationService source, String resource, SerializationEventKind kind) {
		super(source);
		_resource = resource;
		_kind = kind;
	}
	
	public String getResource(){
		return _resource;
	}
	
	public SerializationEventKind getKind(){
		return _kind;
	}

}
