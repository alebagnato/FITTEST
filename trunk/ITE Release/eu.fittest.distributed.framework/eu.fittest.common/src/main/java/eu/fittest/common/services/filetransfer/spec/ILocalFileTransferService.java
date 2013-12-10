package eu.fittest.common.services.filetransfer.spec;


import eu.fittest.common.core.service.ILocalService;


public interface ILocalFileTransferService extends ILocalService {

    
    void addServiceListener(final IFileTransferServiceListener listener);

    
    void removeServiceListener(final IFileTransferServiceListener listener);

}
