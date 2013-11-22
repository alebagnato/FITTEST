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

import java.util.Iterator;
import org.fruit.Assert;
import org.fruit.UnFunc;
import org.fruit.Util;

public class TrajectoryVisualizer implements Visualizer {

	private static final long serialVersionUID = 1107281202398264314L;
	final UnFunc<State, Iterable<Point>> trajectory;
	final Pen pen;

	public TrajectoryVisualizer(Pen pen, Position... positions){
		this(new SplineTrajectory(10, positions), pen);
	}
	
	public TrajectoryVisualizer(UnFunc<State, Iterable<Point>> trajectory, Pen pen){
		Assert.notNull(trajectory, pen);
		Assert.isTrue(pen.strokeWidth() != null);
		this.trajectory = trajectory;		
		this.pen = pen;
	}
	
	public void run(State s, Canvas c, Pen pen) {
		Assert.notNull(s, c, pen);
		pen = Pen.merge(pen, this.pen);
		Iterator<Point> iter = trajectory.apply(s).iterator();
		Point last = iter.next();
		
		while(iter.hasNext()){
			Point current = iter.next();
			
			if(!iter.hasNext() && (pen.strokeCaps() == StrokeCaps._Arrow || pen.strokeCaps() == StrokeCaps.Arrow_))
				Util.arrow(c, pen, last.x(), last.y(), current.x(), current.y(), 5 * pen.strokeWidth(), 5 * pen.strokeWidth());
			else
				c.line(pen, last.x(), last.y(), current.x(), current.y());
			
			last = current;
		}
	}
}
