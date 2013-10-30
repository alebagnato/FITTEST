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
import org.fruit.Assert;
import org.fruit.UnFunc;

public final class DynWidgetDesc implements UnFunc<SUT, String>, Serializable {

	private static final long serialVersionUID = 3603695549691732394L;
	Finder finder;
	String formatString;
	Tag<?>[] tags;
	
	public DynWidgetDesc(Finder finder, String formatString, Tag<?>... tags){
		Assert.notNull(formatString, finder, tags);
		this.formatString = formatString;
		this.finder = finder;
		this.tags = tags;
	}
	
	public String apply(SUT s) {
		Assert.notNull(s);
		Widget w = finder.apply(s.get(Tags.SystemState));
		Object[] tagValues = new Object[tags.length];
		for(int i = 0; i < tags.length; i++)
			tagValues[i] = w.get(tags[i], null);
		return String.format(formatString, tagValues);
	}
}
