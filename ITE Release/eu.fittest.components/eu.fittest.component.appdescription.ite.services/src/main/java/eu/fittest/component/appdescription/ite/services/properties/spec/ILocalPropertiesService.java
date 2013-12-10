package eu.fittest.component.appdescription.ite.services.properties.spec;

import eu.fittest.common.core.service.ILocalService;

public interface ILocalPropertiesService extends ILocalService {
	String getProperty(String name);
	void setProperty(String name, String value);
	boolean isPropertySet(String name);
	void unsetProperty(String name);
}
