package eu.fittest.common.core.identity.spec;

import java.util.UUID;

public class FITTESTEntity {
	private String _id;
	
	public FITTESTEntity(String id){
		if(id == null){
			_id = this.getClass().getSimpleName() +"-"+UUID.randomUUID().toString();
		}
		else{
			_id = id;
		}
	}
	
	public FITTESTEntity(){
		this(null);
	}
	
	public String getId(){
		return _id;
	}
	
	
	public int hashCode() {
		return _id.hashCode();
	}
	
	
	public boolean equals(Object obj) {
		if(!(obj instanceof FITTESTEntity)) return false;
		else {
			return ((FITTESTEntity)obj)._id.equals(_id);
		}
	}
	
	
	public String toString() {
		return _id;
	}
}
