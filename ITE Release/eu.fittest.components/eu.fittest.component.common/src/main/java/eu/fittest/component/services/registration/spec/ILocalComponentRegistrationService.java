package eu.fittest.component.services.registration.spec;

import eu.fittest.common.core.service.ILocalService;

public interface ILocalComponentRegistrationService extends ILocalService{
	String getIteId();
	/**
	 * @return a file URI as a string
	 */
	String getComponentDir();
	String getAgentId();
}
