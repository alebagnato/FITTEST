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
package org.fruit.alayer.actions;

import org.fruit.Assert;
import org.fruit.Util;
import org.fruit.alayer.AbsolutePosition;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Action;
import org.fruit.alayer.Point;
import org.fruit.alayer.Position;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.NoSuchTagException;
import org.fruit.alayer.PositionException;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.Tags;

public final class MouseMove extends TaggableBase implements Action {

	private static final long serialVersionUID = 3689287467588080030L;
	private final Position position;
	private final double minDuration;
	
	public MouseMove(Point point){
		this(new AbsolutePosition(point), 0);
	}
	
	public MouseMove(double x, double y){
		this(new AbsolutePosition(x, y), 0);
	}
	
	public MouseMove(Position position){ this(position, 0); }
	
	public MouseMove(Position position, double minDuration){
		Assert.notNull(position);
		Assert.isTrue(minDuration >= 0);
		this.position = position;
		this.minDuration = minDuration;
	}
		
	public String toString() {
		return "Move mouse to " + position.toString() + ".";
	}

	public void run(SUT system, State state, double duration) {
		try{
			Assert.notNull(system, state);
			Point p = position.apply(state);
			Util.moveCursor(system.get(Tags.StandardMouse), p.x(), p.y(), Math.max(duration, minDuration));
		}catch(NoSuchTagException tue){
			throw new ActionFailedException(tue);
		}catch(PositionException pe){
			throw new ActionFailedException(pe);
		}
	}	
}
