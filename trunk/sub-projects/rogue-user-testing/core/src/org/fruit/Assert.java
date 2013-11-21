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

public final class Assert {
	private Assert(){}
	
	public static void isTrue(boolean expression, String text){
		if(!expression)
			throw new IllegalArgumentException(text);
	}
	
	public static void isTrue(boolean expression){
		if(!expression)
			throw new IllegalArgumentException("You passed illegal parameters!");
	}
		
	public static void notNull(Object object, String text){
		if(object == null)
			throw new IllegalArgumentException(text);
	}

	public static <T> T notNull(T object){
		if(object == null)
			throw new IllegalArgumentException("You passed null as a parameter!");
		return object;
	}
	
	public static void notNull(Object o1, Object o2){
		if(o1 == null || o2 == null)
			throw new IllegalArgumentException("You passed null as a parameter!");
	}

	public static void notNull(Object o1, Object o2, Object o3){
		if(o1 == null || o2 == null || o3 == null)
			throw new IllegalArgumentException("You passed null as a parameter!");
	}

	public static void hasText(String string){
		if(string == null || string.length() == 0)
			throw new IllegalArgumentException("You passed a null or empty string!");
	}
}
