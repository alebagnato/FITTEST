package eu.fittest.agent.ite.services.registration.spec;


import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.services.registration.spec.RegistrationData;


public class RegistrationEvent extends ServiceEvent<ILocalRegistrationService> {
    
    public RegistrationData _registrationData;

    
    public RegistrationEventKind _kind;


    
    public RegistrationEvent(final ILocalRegistrationService source, final RegistrationData data, final RegistrationEventKind kind) {
        super(source);
        _registrationData = data;
        _kind = kind;
    }

    
    public RegistrationData getRegistrationData() {
        return _registrationData;
    }

    
    public RegistrationEventKind getKind() {
        return _kind;
    }

}
