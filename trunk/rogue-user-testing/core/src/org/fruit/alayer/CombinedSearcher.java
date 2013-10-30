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
import org.fruit.UnFunc;

public final class CombinedSearcher implements Searcher {

	private static final class MyVisitor implements UnFunc<Widget, SearchFlag>{
		Searcher nextSearcher;
		UnFunc<Widget, SearchFlag> nextVisitor;
		public SearchFlag apply(Widget widget) {
			return nextSearcher.apply(widget, nextVisitor);
		}
	}
	
	private static final long serialVersionUID = -7319307314114534139L;
	final Searcher searcher1, searcher2;
	transient MyVisitor myVisitor;
	
	public CombinedSearcher(Searcher searcher1, Searcher searcher2){
		Assert.notNull(searcher1, searcher2);
		this.searcher1 = searcher1;
		this.searcher2 = searcher2;
	}
	
	public SearchFlag apply(Widget widget, UnFunc<Widget, SearchFlag> visitor) {
		if(myVisitor == null){
			myVisitor = new MyVisitor();
			myVisitor.nextSearcher = searcher2;
		}
		myVisitor.nextVisitor = visitor;
		return searcher1.apply(widget, myVisitor);
	}
}
