package eu.fittest.component.appdescription.ite.services.properties.spec;

import eu.fittest.common.core.service.IRemoteService;

public interface IRemotePropertiesService extends IRemoteService {
	String getProperty(String name);
	void setProperty(String name, String value);
}
