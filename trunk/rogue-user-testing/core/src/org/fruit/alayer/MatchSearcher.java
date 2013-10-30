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
import org.fruit.Util;

public final class MatchSearcher implements Searcher {

	private static final long serialVersionUID = -4983358364829251061L;
	private final UnFunc<Widget, Boolean> matcher;
	
	public MatchSearcher(UnFunc<Widget, Boolean> matcher){
		Assert.notNull(matcher);
		this.matcher = matcher;
	}
		
	public SearchFlag apply(Widget start, UnFunc<Widget, SearchFlag> visitor) {
		Assert.notNull(start);
		
		for(Widget w : Util.makeIterable(start)){
			if(matcher.apply(w)){
				if(visitor.apply(w) == SearchFlag.Stop)
					return SearchFlag.Stop;
			}
		}
		return SearchFlag.OK;
	}
	
	public String toString(){ return "MatchSearcher " + matcher.toString(); }
}
