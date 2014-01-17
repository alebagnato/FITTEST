package eu.fittest.common.core.service;

import java.util.ArrayList;
import java.util.List;



public abstract class AbstractRemoteService implements IRemoteService {
    
    protected List<IMessageHandler> _handlers;


    
    protected AbstractRemoteService() {
        _handlers = new ArrayList<IMessageHandler>();
    }

    
    
    public List<IMessageHandler> getHandlers() {
        return _handlers;
    }

}
