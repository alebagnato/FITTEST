package eu.fittest.common.services.command.spec;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.ILocalService;




public interface ILocalCommandService extends ILocalService{

    
    void addCommandServiceListener(final ICommandServiceListener listener);

    
    void removeCommandServiceListener(final ICommandServiceListener listener);
    
    void killAllRunningTasks() throws FITTESTException;

}
