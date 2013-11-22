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

import org.fruit.alayer.Role;

public final class ActionRoles {
	private ActionRoles(){}

	public static final Role

	Action = Role.from("Action"),
	MouseAction = Role.from("MouseAction", Action),
	KeyboardAction = Role.from("KeyboardAction", Action),
	MouseMove = Role.from("MouseMove", MouseAction),
	MouseDown = Role.from("MouseDown", MouseAction),
	KeyDown = Role.from("KeyDown", KeyboardAction),
	MouseUp = Role.from("MouseUp", MouseAction),
	KeyUp = Role.from("KeyUp", KeyboardAction),
	HitKey = Role.from("HitKey", KeyDown, KeyUp),
	Click = Role.from("Click", MouseDown, MouseUp),
	LeftClick = Role.from("LeftClick", Click),
	RightClick = Role.from("RightClick", Click),
	DoubleClick = Role.from("DoubleClick", Click),
	LDoubleClick = Role.from("LDoubleClick", LeftClick, DoubleClick),
	RDoubleClick = Role.from("RDoubleClick", RightClick, DoubleClick),
	ClickAt = Role.from("ClickAt", Click, MouseMove),
	LeftClickAt = Role.from("LeftClickAt", ClickAt, LeftClick),
	RightClickAt = Role.from("RightClickAt", ClickAt, RightClick),
	DoubleClickAt = Role.from("DoubleClickAt", ClickAt, DoubleClick),
	LDoubleClickAt = Role.from("LDoubleClickAt", DoubleClickAt, LeftClick),
	RDoubleClickAt = Role.from("RDoubleClickAt", DoubleClickAt, RightClick),
	Type = Role.from("Type", HitKey),
	ClickTypeInto = Role.from("ClickTypeInto", ClickAt, Type),
	Drag = Role.from("Drag", MouseDown, MouseUp, MouseMove),
	LeftDrag = Role.from("LeftDrag", Drag);
}
