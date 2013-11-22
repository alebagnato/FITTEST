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
import org.fruit.Util;

/**
 * A Verdict is the outcome of a test oracle. It determines whether an <code>SUT</code>'s state is erroneous and if so
 * provides a short explanation and a visualization of what is wrong. 
 */
public final class Verdict implements Serializable {
	private static final long serialVersionUID = 3517681535425699094L;
	public static final Verdict OK = new Verdict(0.0, "No problem detected.", Util.NullVisualizer);
	private final String info;
	private final double severity;
	private final Visualizer visualizer;
	
	public Verdict (double severity, String info){
		this(severity, info, Util.NullVisualizer);
	}
	
	public Verdict(double severity, String info, Visualizer visualizer){
		Assert.isTrue(severity >= 0 && severity <= 1.0);
		Assert.notNull(info, visualizer);
		this.severity = severity;
		this.info = info;
		this.visualizer = visualizer;
	}

	/**
	 * returns the likelihood of the state to be erroneous (value within interval [0, 1])
	 * @return value within [0, 1]
	 */
	public double severity(){ return severity; }
	
	/**
	 * returns a short description about whether the state is erroneous and if so, what part of it
	 * @return
	 */
	public String info(){ return info; }
	
	/**
	 * This visualizer should visualize the part of the state where the problem occurred.
	 * For example: If there is a suspicious control element, like f.e. a critical message box
	 * than this should be framed or pointed to with a big red arrow. 
	 * @return the visualizer which is guaranteed to be non-null
	 */
	public Visualizer visualizer(){ return visualizer; }
	public String toString(){ return "severity: " + severity + " info: " + info; }
}
