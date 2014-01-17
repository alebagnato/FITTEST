package eu.fittest.common.services.filetransfer.spec;


import eu.fittest.common.core.service.ServiceEvent;


public class FileEvent extends ServiceEvent<ILocalFileTransferService> {
    
    private String _path;

    
    private FileEventKind _kind;


    
    public FileEvent(ILocalFileTransferService source, FileEventKind kind, String path) {
        super(source);
        _kind = kind;
        _path = path;
    }

    
    public String getPath() {
        return _path;
    }

    
    public FileEventKind getKind() {
        return _kind;
    }

}
