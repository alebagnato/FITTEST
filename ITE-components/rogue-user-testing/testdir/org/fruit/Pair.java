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
package org.fruit;

import java.io.Serializable;
import org.fruit.Util;

public class Pair<L, R> implements Serializable {
	private static final long serialVersionUID = 6777608823096421544L;
	private final L left;
	private final R right;
	
	public static <L, R> Pair<L, R> from(L left, R right){ 
		return new Pair<L, R>(left, right); 
	}

	public Pair(L left, R right){
		this.left = left;
		this.right = right;
	}
	
	public boolean equals(Object o){
		if(this == o)
			return true;
		
		if(o instanceof Pair){
			Pair<?, ?> po = (Pair<?, ?>) o;
			return Util.equals(left, po.left()) && 
					Util.equals(right, po.right());
		}
		
		return false;
	}
	
	public int hashCode(){
		return Util.hashCode(left) + Util.hashCode(right);
	}
	
	public String toString(){
		return "(" + Util.toString(left) + ", " + Util.toString(right) + ")";
	}
	
	public L left(){ return left; }
	public R right(){ return right; }	
}
