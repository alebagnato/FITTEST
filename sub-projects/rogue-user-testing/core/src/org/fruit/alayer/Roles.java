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

public final class Roles {
	private Roles(){}
	
	public static final Role 
	
	Widget = Role.from("Widget"),
	Control = Role.from("Control", Widget),
	Expander = Role.from("Expander", Control),
	Hider = Role.from("Hider", Control),
	Button = Role.from("Button", Control),
	StateButton = Role.from("StateButton", Button),
	ToggleButton = Role.from("ToggleButton", StateButton),
	Item = Role.from("Item", Control),
	ItemContainer = Role.from("ItemContainer", Control),
	Text = Role.from("Text", Control),
	Decoration = Role.from("Decoration", Control),
	Slider = Role.from("Slider", Control),
	Dialog = Role.from("Dialog", Control),
	MessageBox = Role.from("MessageBox", Dialog),
	DragSource = Role.from("DragSource", Control),
	DropTarget = Role.from("DropTarget", Control),
	SUT = Role.from("SUT"),
	System = Role.from("System", Widget),
	Process = Role.from("Process", System);	
}
