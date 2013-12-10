package eu.fittest.component.services.indirection.spec;

import eu.fittest.common.core.service.ILocalService;

public interface ILocalIndirectionService extends ILocalService {
	void addIndirectionServiceListener(IIndirectionServiceListener l);
	void removeIndirectionServiceListener(IIndirectionServiceListener l);
}
