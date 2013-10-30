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

/**
 * Color objects represent a color and are used together with <code>Pen</code>'s and
 * <code>Canvas</code> objects.
 * 
 * @see Pen
 * @see Canvas
 */
public final class Color implements Serializable {
	private static final long serialVersionUID = 7465037502136479951L;

	public static final Color Red = from(255, 0, 0, 255);
	public static final Color DeepPink = from(255, 20, 147, 255);
	public static final Color Aqua = from(0, 128, 255, 255);
	public static final Color Moss = from(0, 128, 64, 255);
	public static final Color Salmon = from(255, 102, 102, 255);
	public static final Color Blue = from(0, 0, 255, 255);
	public static final Color Navy = from(0, 0, 128, 255);
	public static final Color CornflowerBlue = from(100, 149, 237, 255);
	public static final Color SteelBlue = from(70, 130, 180, 255);
	public static final Color BlueViolet = from(138, 43, 226, 255);
	public static final Color Green = from(0, 255, 0, 255);
	public static final Color LimeGreen = from(50, 205, 50, 255);
	public static final Color Yellow = from(255, 255, 0, 255);
	public static final Color Gold = from(255, 215, 0, 255);
	public static final Color White = from(255, 255, 255, 255);
	public static final Color Black = from(0, 0, 0, 255);
	
	public static Color from(int red, int green, int blue, int alpha){ return new Color(red, green, blue, alpha); }

	private final int red, green, blue, alpha, argb32;

	private Color(int red, int green, int blue, int alpha){
		Assert.isTrue(red >= 0 && green >= 0 && blue >= 0 && alpha >= 0 &&
				red <= 255 && green <= 255 && blue <= 255 && alpha <= 255);
		this.red = red;
		this.green = green;
		this.blue = blue;
		this.alpha = alpha;
		this.argb32 = red + (green << 8) + (blue << 16) + (alpha << 24);
	}

	public int red(){ return red; }
	public int green(){ return green; }
	public int blue(){ return blue; }
	public int alpha(){ return alpha; }
	public int argb32(){ return argb32; }
	public int hashCode(){ return argb32();	}

	public boolean equals(Object o){
		if(this == o)
			return true;

		if(o instanceof Color){
			Color co = (Color) o;
			return co.alpha == alpha && co.red == red && co.green == green && co.blue == blue;
		}
		return false;
	}
	
	public String toString(){ 
		return "Color (red: " + red() + " green: " + green() +
			" blue: " + blue() + " alpha: " + alpha() + ")";
	}
}
