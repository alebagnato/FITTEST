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

public final class OrthogonalPosition implements Position {

	private static final long serialVersionUID = -5638599581798926650L;
	final Position pos1, pos2;
	final double relR, absR;
	
	public OrthogonalPosition(Position pos1, Position pos2, double relR, double absR){
		Assert.notNull(pos1, pos2);
		this.pos1 = pos1;
		this.pos2 = pos2;
		this.relR = relR;
		this.absR = absR;
	}
	
	public Point apply(State state) {
		Assert.notNull(state);
		Point p1 = pos1.apply(state);
		Point p2 = pos2.apply(state);
		double centerX = (p1.x() + p2.x()) * .5;
		double centerY = (p1.y() + p2.y()) * .5;
		double l = Util.length(p1.x(), p1.y(), p2.x(), p2.y());
		return Util.OrthogonalPoint(centerX, centerY, p2.x(), p2.y(), relR * l + absR);
	}
}
