/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.alayer;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.fruit.Assert;

public final class Role implements Serializable {
	private static final long serialVersionUID = 4301814192425648282L;	
	private final static ConcurrentHashMap<Role, Role> existingRoles = new ConcurrentHashMap<Role, Role>();
	private Set<Role> parents;
	transient private Set<Role> ancestors;
	private final String name;
	private int hashcode = 0;

	public static boolean isOneOf(Role r, Role... oneOf){
		Assert.notNull(r, oneOf);
		for(Role o : oneOf){
			if(r.isA(o))
				return true;
		}
		return false;
	}
		
	public static Role from(String name, Role... inheritFrom){
		Assert.notNull(name, inheritFrom);
		Role ret = new Role(name, inheritFrom);
		Role existing = existingRoles.putIfAbsent(ret, ret);
		if(existing != null)
			return existing;
		return ret;
	}

	private Role(final String name, final Role... inheritFrom){
		this.name = name;
		parents = new HashSet<Role>();
		ancestors = new HashSet<Role>();
		
		for(Role r : inheritFrom){
			parents.add(r);
			calculateAncestors(r.parents(), ancestors);
		}
		parents.removeAll(ancestors);
		parents = Collections.unmodifiableSet(parents);
		ancestors.addAll(parents);
		ancestors = Collections.unmodifiableSet(ancestors);
	}
	
	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException{
		ois.defaultReadObject();
		ancestors = Collections.unmodifiableSet(calculateAncestors(parents, new HashSet<Role>()));
	}
	
	private Object readResolve() throws ObjectStreamException{
		Role existing = existingRoles.putIfAbsent(this, this);
		return existing == null ? this : existing;
	}
	
	public Set<Role> parents() { return parents; }
	public Iterable<Role> ancestors(){ return ancestors; }
	public boolean isA(Role other) { return equals(other) || ancestors.contains(other); }
	public String toString(){ return name(); }
	public String name() { return name; }
	
	public boolean equals(Object other){
		if(other == this) return true;
		if(other instanceof Role){
			Role otherR = (Role) other;
			return name.equals(otherR.name) &&
					parents.equals(otherR.parents);
		}
		return false;
	}
	
	public int hashCode(){
		int ret = hashcode;
		if(ret == 0){
			ret = name.hashCode() + 31 * parents.hashCode();
			hashcode = ret;
		}
		return ret;			
	}
	
	private Set<Role> calculateAncestors(Set<Role> parents, Set<Role> out){
		for(Role parent : parents){
			out.add(parent);
			calculateAncestors(parent.parents(), out);
		}
		return out;
	}	
}
