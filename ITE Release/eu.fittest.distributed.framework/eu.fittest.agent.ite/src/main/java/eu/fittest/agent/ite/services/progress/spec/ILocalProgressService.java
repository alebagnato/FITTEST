package eu.fittest.agent.ite.services.progress.spec;

import eu.fittest.common.core.service.ILocalService;

public interface ILocalProgressService extends ILocalService {

    void addProgressListener(final IProgressServiceListener listener);

    void removeProgressListener(final IProgressServiceListener listener);

}
