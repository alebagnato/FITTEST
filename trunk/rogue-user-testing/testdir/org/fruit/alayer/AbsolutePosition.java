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
/*
 * This file is part of FRUIT.
 *
 * FRUIT is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * FRUIT is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with FRUIT.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.fruit.alayer;

import org.fruit.Assert;

public final class AbsolutePosition implements Position {
	private static final long serialVersionUID = -6784500620656208720L;
	private final Point p;
	public AbsolutePosition(double x, double y){ p = Point.from(x, y); }
	public AbsolutePosition(Point point){
		Assert.notNull(point);
		p = point;
	}
	public Point apply(State state){ return p; }
	public String toString(){ return p.toString(); }
}
