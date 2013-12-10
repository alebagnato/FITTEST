package eu.fittest.common.services.filetransfer.spec;

import java.io.InputStream;
import java.io.OutputStream;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IRemoteService;


public interface IRemoteFileTransferService extends IRemoteService {

    
    void upload(final InputStream data, final String relativeURI, final long fileLength) throws FITTESTException;
    void upload(final InputStream data, final String relativeURI, final long fileLength, boolean inflase, long checksum) throws FITTESTException;

    
    void download(final OutputStream data, final String absoluteURI) throws FITTESTException;

    
    void delete(final String relativeURI) throws FITTESTException;

}
