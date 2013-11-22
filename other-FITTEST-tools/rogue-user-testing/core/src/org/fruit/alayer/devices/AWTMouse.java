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
package org.fruit.alayer.devices;

import java.awt.AWTException;
import java.awt.MouseInfo;
import java.awt.PointerInfo;
import java.awt.Robot;
import org.fruit.FruitException;
import org.fruit.alayer.Point;

public final class AWTMouse implements Mouse {
	public static AWTMouse build() throws FruitException{ return new AWTMouse(); }
	private final Robot robot;

	private AWTMouse() throws FruitException{
		try{
			robot = new Robot();
		}catch(AWTException awte){
			throw new FruitException(awte);
		}
	}

	public String toString() { return "AWT Mouse"; }
	public void press(MouseButtons k) { robot.mousePress(k.code()); }
	public void release(MouseButtons k) { robot.mouseRelease(k.code()); }

	public void isPressed(MouseButtons k) {
		throw new UnsupportedOperationException("AWT Mouse cannot poll the mouse's state!");
	}

	public void setCursor(double x, double y) { robot.mouseMove((int)x, (int)y); }

	public Point cursor() {
		PointerInfo info = MouseInfo.getPointerInfo();
		if(info == null)
			throw new RuntimeException("MouseInfo.getPointerInfo() returned null! This seeems to be undocumented Java library behavior... " +
					"Consider using a platform specific Mouse Implementation instead of AWTMouse!");
		java.awt.Point p = info.getLocation();
		Point ret = Point.from(p.x, p.y);
		return ret;
	}
}
