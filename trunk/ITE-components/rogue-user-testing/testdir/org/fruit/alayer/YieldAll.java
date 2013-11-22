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

import java.util.HashSet;
import java.util.Set;
import org.fruit.UnFunc;

public final class YieldAll implements UnFunc<Widget, SearchFlag> {
	private final Set<Widget> widgets = new HashSet<Widget>();

	public SearchFlag apply(Widget widget) {
		widgets.add(widget);
		return SearchFlag.OK;
	}
	public Set<Widget> result(){ return widgets; }
}
