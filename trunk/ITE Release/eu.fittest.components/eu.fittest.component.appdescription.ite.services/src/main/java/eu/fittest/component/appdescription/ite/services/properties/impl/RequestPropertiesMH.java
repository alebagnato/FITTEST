package eu.fittest.component.appdescription.ite.services.properties.impl;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.PropertiesResponse;
import eu.fittest.common.core.xml.RequestProperties;
import eu.fittest.common.core.xml.RequestProperties.Property;
import eu.fittest.common.services.message.AbstractMessageHandler;

import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;

public class RequestPropertiesMH extends AbstractMessageHandler<IPropertiesService> {

	protected RequestPropertiesMH(IPropertiesService service) {
		super(service);	
	}

	public synchronized void onReception(Connection connection, RequestProperties message) throws FITTESTException {
		PropertiesResponse response = FITTESTSingleton.getObjectFactory().createPropertiesResponse();
		for(Property p : message.getProperty()){
			eu.fittest.common.core.xml.PropertiesResponse.Property pr = FITTESTSingleton.getObjectFactory().createPropertiesResponseProperty();
			pr.setValue(_service.getProperty(p.getName()));
			pr.setName(p.getName());
			response.getProperty().add(pr);
		}
		reply(connection, message, response);
	}
}
