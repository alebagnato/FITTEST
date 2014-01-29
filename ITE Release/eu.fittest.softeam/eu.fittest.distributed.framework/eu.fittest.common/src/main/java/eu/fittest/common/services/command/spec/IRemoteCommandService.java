package eu.fittest.common.services.command.spec;

import java.net.URI;
import java.util.Map;

import eu.fittest.common.core.exception.FITTESTException;


public interface IRemoteCommandService {

    
    String execute(final String command, final Map<String, String> parameters, final Map<String, String> environment, final URI workingDirFileURI) throws FITTESTException;

    
    void kill(final String taskID) throws FITTESTException;

}
