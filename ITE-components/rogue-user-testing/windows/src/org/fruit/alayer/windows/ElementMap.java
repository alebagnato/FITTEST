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
package org.fruit.alayer.windows;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.fruit.Assert;
import org.fruit.alayer.Rect;

public final class ElementMap implements Serializable {
	private static final long serialVersionUID = 8336577831205889395L;
	final List<UIAElement> elements;

	private static class ElementComp implements Comparator<UIAElement>{
		final static int WORSE = 1, BETTER = -1, EVEN = 0;
		public int compare(UIAElement o1, UIAElement o2) {
			if(o1.zindex < o2.zindex){
				return WORSE;
			}else if (o1.zindex > o2.zindex){
				return BETTER;
			}else{
				if(o1.rect != null){
					if(o2.rect != null){
						double area1 = Rect.area(o1.rect);
						double area2 = Rect.area(o2.rect);
						return area1 < area2 ? BETTER : (area1 > area2 ? WORSE : EVEN);
					}else{
						return BETTER;
					}
				}else{
					return WORSE;
				}
			}
		}
	}

	public static Builder newBuilder(){ return new Builder(); }

	public static final class Builder{
		final List<UIAElement> elements = new ArrayList<UIAElement>();

		public Builder addElement(UIAElement element){
			Assert.notNull(element);
			if(element.rect != null)
				elements.add(element);		
			return this;
		}

		public ElementMap build(){
			Collections.sort(elements, new ElementComp());
			return new ElementMap(this);
		}
	}


	private ElementMap(Builder builder){
		this.elements = builder.elements;
	}

	public UIAElement at(double x, double y){
		for(UIAElement element : elements){
			if(element.rect.contains(x, y))
				return element;
		}
		return null;
	}

	public boolean obstructed(UIAElement element, double x, double y){
		for(UIAElement obstacle : elements){
			if(obstacle.zindex <= element.zindex || obstacle == element)
				break;
			if(obstacle.rect.contains(x, y))
				return true;
		}
		return false;
	}
}
