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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Map;
import org.fruit.Util;

final class UIARootElement extends UIAElement {
	private static final long serialVersionUID = -2561441199642411403L;
	long pid, timeStamp;
	boolean isRunning, isForeground, hasStandardMouse, hasStandardKeyboard;	
	transient Map<Long, UIAElement> hwndMap;
	ElementMap tlc;

	public UIARootElement(){
		super(null);
		root = this;
		hwndMap = Util.newHashMap();
		tlc = ElementMap.newBuilder().build();
	}

	public UIAElement at(double x, double y){
		throw new UnsupportedOperationException();
	}

	public boolean visibleAt(UIAElement el, double x, double y){		
		if(el.rect == null || !el.rect.contains(x, y) || !this.rect.contains(x, y))
			return false;
		UIAElement topLevelContainer = tlc.at(x, y);
		return (topLevelContainer == null || topLevelContainer.zindex <= el.zindex) && !obscuredByChildren(el, x, y);
	}

	boolean obscuredByChildren(UIAElement el, double x, double y){
		for(int i = 0; i < el.children.size(); i++){
			UIAElement child = el.children.get(i);
			if(child.rect != null && child.rect.contains(x, y) && child.zindex >= el.zindex)
				return true;
		}
		return false;
	}

	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException{
		ois.defaultReadObject();
	}

	private void writeObject(ObjectOutputStream oos) throws IOException{
		oos.defaultWriteObject();
	}
}
