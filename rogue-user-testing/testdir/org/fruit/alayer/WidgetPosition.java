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
import org.fruit.Util;

public final class WidgetPosition implements Position {

	private static final long serialVersionUID = -6963490602249863461L;
	private final Finder finder;
	private final double relX, relY;
	private final Tag<? extends Shape> shapeTag;
	private final boolean hitTest;

	public static WidgetPosition fromFinder(Finder finder){
		return fromFinder(finder, 0.5, 0.5);
	}

	public static WidgetPosition fromFinder(Finder finder, double relX, double relY){
		return new WidgetPosition(finder, Tags.Shape, relX, relY, true);
	}

	public WidgetPosition(Finder finder, Tag<? extends Shape> shapeTag, double relX, double relY, boolean hitTest){
		Assert.notNull(finder, shapeTag);		
		this.shapeTag = shapeTag;
		this.finder = finder;
		this.relX = relX;
		this.relY = relY;
		this.hitTest = hitTest;
	}

	public Point apply(State state) throws PositionException {
		try{
			Widget widget = finder.apply(state);
			if(hitTest && !Util.hitTest(widget, relX, relY))
				throw new PositionException("Widget found, but hittest failed!");
			return Util.relToAbs(widget.get(shapeTag), relX, relY);
		}catch(WidgetNotFoundException wnfe){
			throw new PositionException(wnfe);
		}catch(NoSuchTagException pue){
			throw new PositionException(pue);
		}
	}

	public String toString(){ return "WidgetPosition (" + relX + ", " + relY + ")"; }
}
