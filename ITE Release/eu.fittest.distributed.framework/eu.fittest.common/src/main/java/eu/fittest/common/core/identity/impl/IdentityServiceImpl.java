package eu.fittest.common.core.identity.impl;

import eu.fittest.common.core.identity.spec.FITTESTEntity;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.AbstractService;

public class IdentityServiceImpl extends AbstractService implements IIdentityService{
	private FITTESTEntity _entity;
	
	public IdentityServiceImpl(){
		this(new FITTESTEntity());
	}
	
	public IdentityServiceImpl(FITTESTEntity entity){
		_entity = entity;
	}
	
	
	public String getName() {
		return IIdentityService.class.getName();
	}

	
	public String getMyIdentity() {
		return _entity.getId();
	}

	
	public void setMyIdentity(FITTESTEntity entity) {
		_entity = entity;		
	}

}
