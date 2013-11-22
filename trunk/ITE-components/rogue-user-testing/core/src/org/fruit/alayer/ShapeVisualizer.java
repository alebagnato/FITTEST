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

public final class ShapeVisualizer implements Visualizer {
	private static final long serialVersionUID = -1411595441118761574L;
	private final Shape shape;
	private final String label;
	private final double labelX, labelY;
	private final Pen pen;
	
	public ShapeVisualizer(Pen pen, Shape shape, String label, double labelX, double labelY){
		Assert.notNull(shape, pen);
		this.shape = shape;
		this.pen = pen;
		this.label = label;
		this.labelX = labelX;
		this.labelY = labelY;
	}

	public void run(State state, Canvas c, Pen pen) {
		Assert.notNull(state, c, pen);
		pen = Pen.merge(pen, this.pen);
		shape.paint(c, pen);		
		if(label != null)
			c.text(pen, shape.x() + shape.width() * labelX, shape.y() + shape.height() * labelY, 0, label);
	}
}
