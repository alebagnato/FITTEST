package eu.fittest.common.core.registry;

import java.util.HashMap;
import java.util.Map;

import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.core.service.IService;
import eu.fittest.common.core.service.IServiceRegistry;


public class ServiceRegistryImpl extends AbstractService implements IServiceRegistry {
    
    private Map<String , IService> _services;

    public void registerService(final IService service) {
    	IService old = _services.get(service.getName());
    	if(old!=null){
    		fireEvent(new ServiceRegistryEvent(this, old, ServiceRegistryEventKind.SERVICE_REMOVED));
    	}
        _services.put(service.getName(), service);
        service.setRegistry(this);
        fireEvent(new ServiceRegistryEvent(this, service, ServiceRegistryEventKind.SERVICE_ADDED));
    }

    
    public ILocalService findLocalService(final String serviceName) {
        return _services.get(serviceName);
    }

    
    
    public String getName() {
        return IServiceRegistry.class.getName();
    }

    
    
    public <T extends eu.fittest.common.core.service.IService> T findService(Class<T> serviceName) {
        return serviceName.cast(_services.get(serviceName.getName()));
    }

    
    public ServiceRegistryImpl() {
        _services = new HashMap<String, IService> ();
        registerService(this);
    }

}
