package eu.fittest.component.appdescription.ite.services.properties.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.PropertiesResponse;
import eu.fittest.common.core.xml.PropertiesResponse.Property;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;

public class PropertiesResponseMH extends AbstractMessageHandler<IPropertiesService>{

	protected PropertiesResponseMH(IPropertiesService service) {
		super(service);
	}

	public synchronized void onReception(Connection connection, PropertiesResponse message) throws FITTESTException {
		for(Property p : message.getProperty()){
			_service.setProperty(p.getName(), p.getValue());
		}
	}
}
