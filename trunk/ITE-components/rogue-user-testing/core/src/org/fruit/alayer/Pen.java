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
import org.fruit.Assert;

/**
 * A pen is used to draw on a <code>Canvas</code> object. It determines
 * attributes such as line width, color and fill pattern.
 * 
 * @see Canvas
 */
public abstract class Pen implements Serializable {	
	private static final long serialVersionUID = 8309231237087967825L;

	public static final Pen IgnorePen = Pen.newPen().build();
	public static final Pen FillPen = Pen.newPen().setFillPattern(FillPattern.Solid).build();
	public static final Pen StrokePen = Pen.newPen().setStrokePattern(StrokePattern.Solid)
			.setFillPattern(FillPattern.None).build();
	public static final Pen DefaultPen = Pen.newPen().setStrokePattern(StrokePattern.Solid)
			.setFillPattern(FillPattern.None).setColor(Color.CornflowerBlue).setFont("Times New Roman")
			.setFontSize(16).setStrokeCaps(StrokeCaps.None).setStrokeWidth(5).build();

	public static Builder newPen(){ return new Builder(null); }
	public static Builder startFrom(Pen base){ return new Builder(base); }
	public static Pen merge(Pen priorityPen, Pen backupPen){ return new MergedPen(priorityPen, backupPen); }

	public static final class Builder{
		Color color;
		Double strokeWidth, fontSize;
		String font;
		StrokePattern strokePattern;
		FillPattern fillPattern;
		StrokeCaps strokeCaps;
		Pen base;

		private Builder(Pen base){ this.base = base; }

		public Builder setColor(Color c){
			Assert.notNull(c);
			color = c;
			return this;
		}

		public Builder setFont(String font){
			Assert.notNull(font);
			this.font = font;
			return this;
		}

		public Builder setStrokeWidth(double width){
			Assert.isTrue(width >= 0);
			strokeWidth = width;
			return this;
		}

		public Builder setFontSize(double size){
			Assert.isTrue(size >= 0);
			fontSize = size;
			return this;
		}

		public Builder setStrokePattern(StrokePattern pattern){
			Assert.notNull(pattern);
			this.strokePattern = pattern;
			return this;
		}

		public Builder setFillPattern(FillPattern pattern){
			Assert.notNull(pattern);
			this.fillPattern = pattern;
			return this;
		}

		public Builder setStrokeCaps(StrokeCaps caps){
			Assert.notNull(caps);
			this.strokeCaps = caps;
			return this;
		}

		public Pen build(){
			return base == null ? new StdPen(this) : merge(new StdPen(this), base);
		}
	}
	
	private Pen(){}
	public abstract Color color();
	public abstract Double strokeWidth();
	public abstract Double fontSize();
	public abstract String font();
	public abstract StrokePattern strokePattern();
	public abstract FillPattern fillPattern();
	public abstract StrokeCaps strokeCaps();


	private static final class StdPen extends Pen{
		private static final long serialVersionUID = -7928196200263289513L;
		final Color color;
		final Double strokeWidth, fontSize;
		final String font;
		final StrokePattern strokePattern;
		final FillPattern fillPattern;
		final StrokeCaps strokeCaps;

		private StdPen(Builder b){
			color = b.color;
			strokeWidth = b.strokeWidth;
			fontSize = b.fontSize;
			font = b.font;
			strokeCaps = b.strokeCaps;
			fillPattern = b.fillPattern;
			strokePattern = b.strokePattern;
		}

		public Color color(){ return color; }
		public Double strokeWidth(){ return strokeWidth; }
		public Double fontSize(){ return fontSize; }
		public String font(){ return font; }
		public StrokePattern strokePattern(){ return strokePattern; }
		public FillPattern fillPattern(){ return fillPattern; }
		public StrokeCaps strokeCaps(){ return strokeCaps; }
	}
	
	
	private final static class MergedPen extends Pen{
		private static final long serialVersionUID = 6526439728542225584L;
		final Pen pen1, pen2;
		private MergedPen(Pen pen1, Pen pen2){
			Assert.notNull(pen1, pen2);
			this.pen1 = pen1;
			this.pen2 = pen2;
		}
		public Color color() {
			Color c = pen1.color();
			return c == null ? pen2.color() : c;
		}
		public Double strokeWidth() {
			Double sw = pen1.strokeWidth();
			return sw == null ? pen2.strokeWidth() : sw;
		}
		public Double fontSize() {
			Double fs = pen1.fontSize();
			return fs == null ? pen2.fontSize() : fs;
		}
		public String font() {
			String f = pen1.font();
			return f == null ? pen2.font() : f;
		}
		public FillPattern fillPattern() {
			FillPattern fp = pen1.fillPattern();
			return fp == null ? pen2.fillPattern() : fp;
		}
		public StrokePattern strokePattern() {
			StrokePattern sp = pen1.strokePattern();
			return sp == null ? pen2.strokePattern() : sp;
		}
		public StrokeCaps strokeCaps() {
			StrokeCaps sc = pen1.strokeCaps();
			return sc == null ? pen2.strokeCaps() : sc;
		}
	}
}
