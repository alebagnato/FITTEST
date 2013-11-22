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

import java.util.Arrays;
import java.util.List;
import org.fruit.Assert;
import org.fruit.Util;
import org.fruit.alayer.Action;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.TaggableBase;

/**
 * An action that is composed of several other actions.
 */
public final class CompoundAction extends TaggableBase implements Action {

	private static final long serialVersionUID = -5836624942752268573L;
	private final List<Action> actions;
	private final List<Double> relativeDurations;
	
	public static final class Builder{
		private List<Double> relativeDurations = Util.newArrayList();
		private List<Action> actions = Util.newArrayList();
		double durationSum = 0.0;
		
		public Builder add(Action a, double relativeDuration){
			Assert.notNull(a);
			Assert.isTrue(relativeDuration >= 0);
			relativeDurations.add(relativeDuration);
			actions.add(a);
			durationSum += relativeDuration;
			return this;
		}
				
		public CompoundAction build(){
			Assert.isTrue(durationSum > 0.0, "Sum of durations needs to be larger than 0!");

			// normalize
			for(int i = 0; i < relativeDurations.size(); i++)
				relativeDurations.set(i, relativeDurations.get(i) / durationSum);
			return new CompoundAction(this);
		}
	}
	
	private CompoundAction(Builder b){
		this.actions = b.actions;
		this.relativeDurations = b.relativeDurations;
	}
	
	public CompoundAction(Action...actions){
		Assert.notNull((Object)actions);
		this.actions = Arrays.asList(actions);
		this.relativeDurations = Util.newArrayList();
		
		if(actions.length > 0){
			for(int i = 0; i < actions.length; i++)
				relativeDurations.add(1.0 / actions.length);
		}
	}
			
	public void run(SUT system, State state, double duration) {		
		for(int i = 0; i < actions.size(); i++)
			actions.get(i).run(system, state, relativeDurations.get(i) * duration);
	}
		
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append("Compound Action =");
		for(Action a : actions)
			sb.append(Util.lineSep()).append(a.toString());
		return sb.toString();
	}
}
