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

import org.fruit.Assert;

public final class FirstFinder implements Finder {
	private static final long serialVersionUID = 4076359312733416905L;
	final Searcher searcher;
	transient YieldFirst yf;
	
	public FirstFinder(Searcher searcher){
		Assert.notNull(searcher);
		this.searcher = searcher;
	}
	
	public Widget apply(Widget start) throws WidgetNotFoundException {
		if(yf == null)
			yf = new YieldFirst();
		searcher.apply(start, yf);
		return yf.result();
	}
	
	public String toString(){ return "FirstFinder"; }
}
