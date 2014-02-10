package eu.fittest.component.appdescription.ite.services.properties.impl;

import java.util.HashMap;

import eu.fittest.common.core.service.AbstractService;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;
import eu.fittest.component.appdescription.ite.services.properties.spec.PropertiesEvent;

public class PropertiesServiceImpl extends AbstractService implements IPropertiesService {
	private HashMap<String, String> _properties;
	
	public PropertiesServiceImpl() {
		_handlers.add(new RequestPropertiesMH(this));
		_handlers.add(new PropertiesResponseMH(this));
		_properties = new HashMap<String, String>();
	}
	
	@Override
	public String getName() {
		return IPropertiesService.class.getName();
	}

	@Override
	public synchronized String getProperty(String name) {
		return _properties.get(name);
	}

	@Override
	public synchronized void setProperty(String name, String value) {
		_properties.put(name, value);
		fireEvent(new PropertiesEvent(this, name, value));
	}

	@Override
	public synchronized boolean isPropertySet(String name) {
		return _properties.containsKey(name);
	}

	@Override
	public synchronized void unsetProperty(String name) {
		_properties.remove(name);
	}
}
