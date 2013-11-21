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
package org.fruit.alayer.windows;

import org.fruit.alayer.HitTester;

public final class UIAHitTester implements HitTester {
	private static final long serialVersionUID = 1134479951851719957L;
	private final UIAElement el;
	public UIAHitTester(UIAElement el){	this.el = el; }
	public String toString(){ return "UIAHitTester"; }
	public boolean apply(double x, double y) { return el.root.visibleAt(el, x, y); }
}
