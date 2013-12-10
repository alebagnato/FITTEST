package eu.fittest.component.appdescription.ite.services.properties.spec;

import eu.fittest.common.core.service.ServiceEvent;

public class PropertiesEvent extends ServiceEvent<ILocalPropertiesService>{
	private String _name;
	private String _value;

	public PropertiesEvent(ILocalPropertiesService source, String name, String value) {
		super(source);
		_name = name;
		_value = value;
	}

	public String getName() {
		return _name;
	}
	
	public String getValue(){
		return _value;
	}
	
}
