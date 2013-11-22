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
 * A Widget is usually a control element of an <code>SUT</code>.
 * Widgets have exactly one parent and can have several children.
 * They are attached to a <code>State</code> and form a Widget Tree.
 * In fact a <code>State</code> is a Widget itself and is the root
 * of the Widget Tree.
 * 
 * @see State
 */
public interface Widget extends Taggable, Serializable {
	State root();
	Widget parent();
	Widget child(int i);
	int childCount();
	void remove();
	void moveTo(Widget p, int idx);
	Widget addChild();
}
