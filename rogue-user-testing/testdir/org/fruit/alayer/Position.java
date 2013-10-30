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
 * A position is a function that operates on a state and calculates a concrete point.
 * For example: A position could calculate "The center of the Button with the title 'OK'".
 * In order to do that, the position first needs to find the Button within the widget tree
 * (i.e. the state). Then it will get the button's <code>Shape</code> tag and calculate
 * the center position.
 */
public interface Position extends Serializable {
	Point apply(State state) throws PositionException;
}
