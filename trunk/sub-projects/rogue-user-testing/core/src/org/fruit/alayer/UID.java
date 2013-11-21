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

import java.io.Serializable;

/**
 * This is a tag-interface for UIDs (Unique Identifiers). A unique
 * identifier is... well... unique and can be used to describe f.e. the
 * identity of a particular widget or action. Implementors of IUID
 * should override hashCode() and equals() to achieve this.
 * @author Sebastian Bauersfeld
 *
 */
public interface UID extends Serializable{
	int hashCode();
	boolean equals(Object o);
}
