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

public final class Point implements Shape {
	private static final long serialVersionUID = -8319702986528318327L;
	private final double x, y;
		
	public static Point from(double x, double y){ return new Point(x, y); }
	
	private Point(double x, double y){
		this.x = x;
		this.y = y;
	}
	
	public double x(){ return x; }
	public double y(){ return y; }
	public double width(){ return 0; }
	public double height(){	return 0; }

	public boolean contains(double x, double y) {
		return this.x == x && this.y == y;
	}

	public void paint(Canvas canvas, Pen pen) {
		Assert.notNull(canvas, pen);
		double d = canvas.defaultPen().strokeWidth();
		canvas.ellipse(Pen.merge(pen, Pen.FillPen), x - d * 0.5, y - d * 0.5, d, d);
	}

	public String toString(){ return "(" + x + ", " + y + ")"; }
	
	public boolean equals(Object o){
		if(this == o)
			return true;
		
		if(o instanceof Point){
			Point po = (Point) o;
			return x == po.x && y == po.y;
		}
		return false;
	}
	
	public int hashCode(){
		long ret = Double.doubleToLongBits(x);
		ret ^= Double.doubleToLongBits(y) * 31;
		return (((int) ret) ^ ((int) (ret >> 32)));
	}
}
