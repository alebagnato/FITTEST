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
import org.fruit.Pair;

public final class OverrideCanvas implements Canvas {

	Canvas canvas;
	Pen overridePen;
	
	public OverrideCanvas(Canvas canvas){
		this(canvas, Pen.DefaultPen);
	}
	
	public OverrideCanvas(Canvas canvas, Pen pen){
		Assert.notNull(canvas, pen);
		this.canvas = canvas;
		this.overridePen = pen;
	}
	
	public void setOverridePen(Pen pen){
		Assert.notNull(pen);
		this.overridePen = pen; 
	}
	
	public double width() { return canvas.width(); }
	public double height() {	return canvas.height(); }
	public double x(){ return canvas.x(); }
	public double y(){ return canvas.y(); }
	public void begin() { canvas.begin(); }
	public void end() { canvas.end(); }
	public Pen defaultPen() { return Pen.merge(overridePen, canvas.defaultPen()); }
	public void release() { canvas.release(); }

	public void line(Pen pen, double x1, double y1, double x2, double y2) {
		canvas.line(Pen.merge(overridePen, pen), x1, y1, x2, y2);
	}

	public void text(Pen pen, double x, double y, double angle, String text) {
		canvas.text(Pen.merge(overridePen, pen), x, y, angle, text);
	}

	public Pair<Double, Double> textMetrics(Pen pen, String text) {
		return canvas.textMetrics(Pen.merge(overridePen, pen), text);
	}

	public void clear(double x, double y, double width, double height) {
		canvas.clear(x, y, width, height);
	}

	public void triangle(Pen pen, double x1, double y1, double x2, double y2,
			double x3, double y3) {
		canvas.triangle(Pen.merge(overridePen, pen), x1, y1, x2, y2, x3, y3);
	}

	public void image(Pen pen, double x, double y, double width,
			double height, int[] image, int imageWidth, int imageHeight) {
		canvas.image(Pen.merge(overridePen, pen), x, y, width, height, image, imageWidth, imageHeight);
	}

	public void ellipse(Pen pen, double x, double y, double width,
			double height) {
		canvas.ellipse(Pen.merge(overridePen, pen), x, y, width, height);
	}

	public void rect(Pen pen, double x, double y, double width, double height) {
		canvas.rect(Pen.merge(overridePen, pen), x, y, width, height);
	}
}
