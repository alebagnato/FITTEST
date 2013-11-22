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

public class StdWidget extends TaggableBase implements Widget, Serializable {
	private static final long serialVersionUID = -1508666976791323005L;
		
	public State root() {
		return null;
	}

	public Widget parent() {
		return null;
	}

	public Widget child(int i) {
		return null;
	}

	public int childCount() {
		return 0;
	}

	public void remove() {
	}

	public void moveTo(Widget p, int idx) {
	}

	public Widget addChild() {
		return null;
	}
}
