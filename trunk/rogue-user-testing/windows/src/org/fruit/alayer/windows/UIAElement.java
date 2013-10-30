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
import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import org.fruit.alayer.Rect;

class UIAElement implements Serializable {
	private static final long serialVersionUID = -2561441199642411403L;
	List<UIAElement> children = Collections.emptyList();
	UIAElement parent;
	UIARootElement root;
	UIAWidget backRef;
	boolean blocked, enabled, ignore, isTopmostWnd, 
	isModal, hasKeyboardFocus, isKeyboardFocusable,
	isWndTopMost, isTopLevelContainer;
	long ctrlId, orientation, hwnd, wndInteractionState, wndVisualState;
	Rect rect;
	String name, helpText, automationId, className, providerDesc, frameworkId;
	double zindex;

	public UIAElement(){ this(null); }

	public UIAElement(UIAElement parent){
		this.parent = parent;
		if(parent != null)
			root = parent.root;
		enabled = true;
	}

	private void writeObject(ObjectOutputStream oos) throws IOException{
		oos.defaultWriteObject();
	}

	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException{
		ois.defaultReadObject();
	}
}
