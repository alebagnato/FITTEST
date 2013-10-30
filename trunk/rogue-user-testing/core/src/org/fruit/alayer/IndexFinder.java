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

import java.util.Arrays;
import org.fruit.Assert;
import org.fruit.UnFunc;

public final class IndexFinder implements Searcher, Finder {
	private static final long serialVersionUID = 2879822217515069377L;
	final int indices[];
	transient YieldFirst yf;

	public IndexFinder(int indices[]){
		Assert.notNull(indices);
		this.indices = indices;		
	}

	public SearchFlag apply(Widget start, UnFunc<Widget, SearchFlag> visitor) {
		Assert.notNull(start, visitor);
		Widget current = start;
		for(int idx : indices){
			if(idx >= 0 && idx < current.childCount())
				current = current.child(idx);
			else
				return SearchFlag.OK;
		}
		return visitor.apply(current);
	}

	public String toString(){
		return "IndexSearcher: " + Arrays.toString(indices);
	}

	public Widget apply(Widget start) throws WidgetNotFoundException {
		if(yf == null)
			yf = new YieldFirst();
		apply(start, yf);
		return yf.result();
	}
}
