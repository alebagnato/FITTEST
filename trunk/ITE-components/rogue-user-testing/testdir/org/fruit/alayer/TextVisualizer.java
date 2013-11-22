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

public final class TextVisualizer implements Visualizer {
	
	private static final long serialVersionUID = 9156304220974950751L;
	final Position pos;
	final String text;
	final Pen pen;
	
	public TextVisualizer(Position pos, String text, Pen pen){
		Assert.notNull(pos, text, pen);
		this.pos = pos;
		this.text = text;
		this.pen = pen;
	}
	
	public void run(State state, Canvas cv, Pen pen) {
		Assert.notNull(state, cv, pen);
		pen = Pen.merge(pen, this.pen);
		Point p = pos.apply(state);
		Pair<Double, Double> m = cv.textMetrics(pen, text);
		cv.text(pen, p.x() - m.left() / 2, p.y() - m.right() / 2, 0, text);
	}
}
