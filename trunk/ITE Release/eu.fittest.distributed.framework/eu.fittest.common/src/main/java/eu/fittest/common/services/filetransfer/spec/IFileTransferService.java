package eu.fittest.common.services.filetransfer.spec;


import eu.fittest.common.core.service.IService;


public interface IFileTransferService extends IService, IRemoteFileTransferService, ILocalFileTransferService, IConfigurableFileTransferService {

}
