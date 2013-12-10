package eu.fittest.common.services.filetransfer.spec;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IConfigurableService;

public interface IConfigurableFileTransferService extends IConfigurableService {
	public void setBasePath(String path) throws FITTESTException;
	public String getBasePath();
}
