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

import org.fruit.FruitException;

public class WidgetNotFoundException extends FruitException {
	private static final long serialVersionUID = 7813610847340484828L;
	public WidgetNotFoundException(){ super(""); }
	public WidgetNotFoundException(String message){ this(message, null); }
	public WidgetNotFoundException(String message, Throwable cause){ super(message, cause); }
	public WidgetNotFoundException(Throwable cause){ super(cause); }
}
