package eu.fittest.common.core.server.spec;

import java.net.Socket;

import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.core.service.ServiceEvent;


public class ServerEvent extends ServiceEvent {
    
    private Socket _socket;


    
    public ServerEvent(ILocalService source, Socket socket) {
        super(source);
        _socket = socket;
    }

    
    public Socket getSocket() {
        return _socket;
    }

}
