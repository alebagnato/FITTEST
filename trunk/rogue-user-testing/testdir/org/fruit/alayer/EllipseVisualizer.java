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

public final class EllipseVisualizer implements Visualizer {

	private static final long serialVersionUID = -6006402344810634504L;
	private final double width, height;
	private final Pen pen;
	private final Position position;
	
	public EllipseVisualizer(Position position, Pen pen, double width, double height){
		Assert.notNull(position, pen);
		this.width = width;
		this.height = height;
		this.pen = pen; 
		this.position = position;
	}
	
	public void run(State state, Canvas canvas, Pen pen) {
		Assert.notNull(state, canvas, pen);
		pen = Pen.merge(pen, this.pen);
		Point p = position.apply(state);
		canvas.ellipse(pen, p.x() - width * .5, p.y() - height * .5, width, height);
	}
}
