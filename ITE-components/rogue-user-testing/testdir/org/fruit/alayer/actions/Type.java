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

import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import org.fruit.Assert;
import org.fruit.Util;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Action;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.devices.KBKeys;

/**
 * An action that types a given text on the StandardKeyboard of the SUT.
 */
public final class Type extends TaggableBase implements Action {

	private static final long serialVersionUID = 2555715152455716781L;
	private static final CharsetEncoder asciiEncoder = Charset.forName("US-ASCII").newEncoder();
	private final String text;
	
	public Type(String text){
		Assert.hasText(text);
		checkAscii(text);
		this.text = text;
	}
	
	public void run(SUT system, State state, double duration) throws ActionFailedException {
		Assert.isTrue(duration >= 0);
		Assert.notNull(system);
		
		double d = duration / text.length();
		Action shiftDown = new KeyDown(KBKeys.VK_SHIFT);
		Action shiftUp = new KeyUp(KBKeys.VK_SHIFT);
		for(int i = 0; i < text.length(); i++){
			char c = text.charAt(i);
			boolean shift = false;

			if(Character.isLetter(c)){
				if(Character.isLowerCase(c))
					c = Character.toUpperCase(c);
				else
					shift = true;
			}
			
			KBKeys key = getKey(c);
						
			if(shift)
				shiftDown.run(system, state, .0);
			new KeyDown(key).run(system, state, .0);
			new KeyUp(key).run(system, state, .0);
			if(shift)
				shiftUp.run(system, state, .0);
			Util.pause(d);
		}
	}
		
	public static void checkAscii(String text){
	    if (!asciiEncoder.canEncode(text))
	    	throw new IllegalArgumentException("This string is not an ascii string!");
	}
	
	private KBKeys getKey(char c){
		for(KBKeys k : KBKeys.values())
			if(k.code() == (int) c)
				return k;
		throw new IllegalArgumentException("Unable to find the corresponding keycode for character '" + c + "'!");
	}
	
	public String toString(){ return "Type text '" + text + "'"; }
}
