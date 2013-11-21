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

import java.util.Iterator;
import java.util.List;
import org.fruit.Assert;
import org.fruit.UnFunc;
import org.fruit.Util;
import org.fruit.alayer.AbsolutePosition;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Abstractor;
import org.fruit.alayer.Action;
import org.fruit.alayer.Point;
import org.fruit.alayer.Position;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.Widget;
import org.fruit.alayer.IndexAbstractor;
import org.fruit.alayer.SplineTrajectory;
import org.fruit.alayer.Tags;
import org.fruit.alayer.WidgetPosition;

/**
 * An action which moves the cursor of the StandardMouse of an SUT to a given position.
 */
public final class MouseGesture extends TaggableBase implements Action {

	private static final long serialVersionUID = -6517076000591224414L;
	final UnFunc<State, Iterable<Point>> trajectory;
	final List<Action> actions;
	final List<Position> actionPositions;
	final double minDuration;

	public static final class Builder{
		final Abstractor abstractor;
		int smoothness;
		List<Action> actions = Util.newArrayList();
		List<Position> positions = Util.newArrayList();
		List<Position> actionPositions = Util.newArrayList();
		Position currentPos = null;
		double minDuration;

		public Builder(){ this(new IndexAbstractor()); }

		public Builder(Abstractor abstractor){
			Assert.notNull(abstractor);
			this.abstractor = abstractor;
			smoothness = 10;
			minDuration = 0;
		}

		public Builder setMinDuration(double value){
			Assert.isTrue(value >= 0);
			minDuration = value;
			return this;
		}

		public Builder setSmoothness(int value){
			Assert.isTrue(value >= 0);
			this.smoothness = value;
			return this;
		}

		public Builder moveTo(Position pos){
			Assert.notNull(pos);
			positions.add(pos);
			currentPos = pos;
			return this;
		}

		public Builder moveTo(double x, double y){
			return moveTo(new AbsolutePosition(x, y));
		}

		public Builder moveTo(Widget w){
			return moveTo(w, 0.5, 0.5);
		}

		public Builder moveTo(Widget w, double x, double y){
			Assert.notNull(w);
			return moveTo(new WidgetPosition(abstractor.apply(w), Tags.Shape, x, y, true));
		}

		public Builder perform(Action a){
			Assert.notNull(currentPos, "You first have to define a start position for the gesture!");
			actions.add(a);
			actionPositions.add(currentPos);
			return this;
		}

		public Action build(){
			Assert.isTrue(positions.size() >= 2, "A mouse gesture is defined by at least 2 positions!");
			actions.add(new NOP());         // so that we really go until the end of the movement
			actionPositions.add(currentPos);
			MouseGesture ret = new MouseGesture(new SplineTrajectory(smoothness, 
					positions.toArray(new Position[positions.size()])), actionPositions, actions, minDuration);
			return ret;
		}
	}

	private MouseGesture(UnFunc<State, Iterable<Point>> trajectory, List<Position> actionPositions, 
			List<Action> actions, double minDuration){
		this.trajectory = trajectory;
		this.actionPositions = actionPositions;
		this.actions = actions;
		this.minDuration = minDuration;
	}

	public void run(SUT system, State state, double duration) throws ActionFailedException {
		Assert.notNull(system);
		Assert.isTrue(duration >= 0);

		duration = Math.max(duration, minDuration);
		
		Point[] actionPoints = new Point[actionPositions.size()];
		for(int i = 0; i < actionPoints.length; i++)
			actionPoints[i] = actionPositions.get(i).apply(state);

		Iterable<Point> movePoints = trajectory.apply(state);
		double trLength = length(movePoints);
		double movetimePerUnit = 0;

		if(trLength > 0)
			movetimePerUnit = duration / trLength;

		Iterator<Point> iter = movePoints.iterator();
		Point currentPos = iter.next();
		new MouseMove(currentPos).run(system, state, 0.0);

		for(int i = 0; i < actions.size(); i++){
			Point until = actionPoints[i];

			while(iter.hasNext() && !currentPos.equals(until)){
				Point targetPos = iter.next();
				double segmentLength = Util.length(currentPos.x(), currentPos.y(), targetPos.x(), targetPos.y());
				new MouseMove(targetPos).run(system, state, segmentLength * movetimePerUnit);
				currentPos = targetPos;
			}
			actions.get(i).run(system, state, 0.0);
		}
				
		if(movetimePerUnit == 0)
			Util.pause(duration);
	}

	private double length(Iterable<Point> t){
		double ret = 0;
		Iterator<Point> iter = t.iterator();
		Point last = iter.next();
		while(iter.hasNext()){
			Point current = iter.next();
			ret += Util.length(current.x(), current.y(), last.x(), last.y());
			last = current;
		}
		return ret;
	}
}
