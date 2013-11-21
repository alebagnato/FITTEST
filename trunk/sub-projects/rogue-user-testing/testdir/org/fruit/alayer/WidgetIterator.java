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

import java.util.LinkedList;

import org.fruit.Assert;

public final class WidgetIterator implements java.util.Iterator<Widget> {

	private final LinkedList<Widget> buffer;
	private final Navigator navi;
	
	public WidgetIterator(Widget start){
        this(start, new BFNavigator());
	}

	public WidgetIterator(Widget start, Navigator navi){
		Assert.notNull(start, navi);
		this.buffer = new LinkedList<Widget>();
		this.navi = navi;
		this.buffer.add(start);
		this.navi.run(this.buffer);
	}

	public boolean hasNext() { return !buffer.isEmpty(); }

	public Widget next() {
		Widget ret = buffer.remove();
		if(!buffer.isEmpty())
			navi.run(buffer);
		return ret;
	}

	public void remove() { throw new UnsupportedOperationException(); }
}
