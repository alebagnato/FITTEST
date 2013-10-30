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
 * A Finder's task is to find a particular widget within a widget tree. It starts it's search from a location within the tree
 * (typically the root). Finder's are abstract representations of widgets and implement a particular search strategy, e.g.
 * "find the widget with the title 'Save'" or "the widget which is the 3rd child of another widget of type 'Canvas'".
 * 
 * Finders must be serializable and are often used to implement actions, e.g. "click on the widget with title 'Save'".
 */
public interface Finder extends Serializable {
	
	/**
	 * Apply the search strategy implemented by this finder and start the search from start.
	 * @param start the node from where to start the search
	 * @return a non-null reference to the located widget.
	 * @throws WidgetNotFoundException if no widget has been found
	 */
	Widget apply(Widget start) throws WidgetNotFoundException;
}
