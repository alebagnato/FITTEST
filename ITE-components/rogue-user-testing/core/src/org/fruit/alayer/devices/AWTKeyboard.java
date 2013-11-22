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
import java.awt.Robot;

import org.fruit.FruitException;

public final class AWTKeyboard implements Keyboard {
	public static AWTKeyboard build() throws FruitException{ return new AWTKeyboard(); }
	private final Robot robot;
	
	private AWTKeyboard(){
		try{
			robot = new Robot();
		}catch(AWTException awte){
			throw new FruitException(awte);
		}
	}
	
	public String toString() { return "AWT Keyboard"; }
	public void press(KBKeys k) { robot.keyPress(k.code());	}
	public void release(KBKeys k) { robot.keyRelease(k.code());	}

	public void isPressed(KBKeys k) {
		throw new UnsupportedOperationException("Unfortunately AWT Keyboard cannot poll the keyboard's state!");
	}
}
