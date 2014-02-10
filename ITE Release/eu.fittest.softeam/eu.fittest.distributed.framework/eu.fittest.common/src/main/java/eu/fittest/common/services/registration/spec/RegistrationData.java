package eu.fittest.common.services.registration.spec;

import eu.fittest.common.core.identity.spec.FITTESTEntity;



public class RegistrationData {
    private long _lease;

    private FITTESTEntity _entity;

    private String _iteid;

    public RegistrationData(final long lease, final FITTESTEntity entity, final String iteId) {
        _lease = lease;
        _entity = entity;
        _iteid = iteId;
    }

    public long getLease() {
        return _lease;
    }

    public FITTESTEntity getEntity() {
        return _entity;
    }

    public String getIteid() {
        return _iteid;
    }

    public void setLease(final long lease) {
        _lease = lease;
    }

    
    public boolean equals(final Object obj) {
        if(! (obj instanceof RegistrationData)) return false;
        return ((RegistrationData)obj).getEntity().getId().equals(_entity.getId());//id is not null
    }
    
    
    public int hashCode() {
    	return _entity.getId().hashCode();
    }

}
