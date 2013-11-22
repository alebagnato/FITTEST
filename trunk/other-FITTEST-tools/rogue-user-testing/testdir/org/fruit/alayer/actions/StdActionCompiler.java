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

import org.fruit.Assert;
import org.fruit.alayer.AbsolutePosition;
import org.fruit.alayer.Abstractor;
import org.fruit.alayer.Action;
import org.fruit.alayer.Position;
import org.fruit.alayer.StdAbstractor;
import org.fruit.alayer.Widget;
import org.fruit.alayer.Tags;
import org.fruit.alayer.WidgetPosition;
import org.fruit.alayer.devices.KBKeys;
import org.fruit.alayer.devices.MouseButtons;

public class StdActionCompiler {
	Abstractor abstractor;
	private final Action LMouseDown = new MouseDown(MouseButtons.BUTTON1);
	private final Action RMouseDown = new MouseDown(MouseButtons.BUTTON3);
	private final Action LMouseUp = new MouseUp(MouseButtons.BUTTON1);
	private final Action RMouseUp = new MouseUp(MouseButtons.BUTTON3);
	private final Action NOP = new NOP();

	public StdActionCompiler(){	this(new StdAbstractor()); }

	public StdActionCompiler(Abstractor abstractor){
		this.abstractor = abstractor;
	}

	public Action leftClick(){
		return new CompoundAction.Builder().add(LMouseDown, 0)
				.add(LMouseUp, 0).add(NOP, 1).build();
	}

	public Action rightClick(){
		return new CompoundAction.Builder().add(RMouseDown, 0).
				add(RMouseUp, 0).add(NOP, 1).build();
	}

	public Action leftDoubleClick(){
		Action lc = leftClick();
		return new CompoundAction.Builder().add(lc, 0).
				add(lc, 0).add(NOP, 1).build();
	}

	public Action leftClickAt(Position position){
		Assert.notNull(position);
		return new CompoundAction.Builder().add(new MouseMove(position), 1)
				.add(LMouseDown, 0).add(LMouseUp, 0).build();
	}

	public Action leftClickAt(double absX, double absY){
		return leftClickAt(new AbsolutePosition(absX, absY));
	}

	public Action leftClickAt(Widget w){
		return leftClickAt(w, 0.5, 0.5);
	}

	public Action leftClickAt(Widget w, double relX, double relY){
		return leftClickAt(new WidgetPosition(abstractor.apply(w), Tags.Shape, relX, relY, true));
	}

	public Action rightClickAt(Position position){
		Assert.notNull(position);
		return new CompoundAction.Builder().add(new MouseMove(position), 1)
				.add(RMouseDown, 0).add(RMouseUp, 0).build();
	}

	public Action rightClickAt(double absX, double absY){
		return rightClickAt(new AbsolutePosition(absX, absY));
	}

	public Action rightClickAt(Widget w){
		return rightClickAt(w, 0.5, 0.5);
	}

	public Action rightClickAt(Widget w, double relX, double relY){
		return rightClickAt(new WidgetPosition(abstractor.apply(w), Tags.Shape, relX, relY, true));
	}

	public Action leftDoubleClickAt(Position position){
		Assert.notNull(position);
		return new CompoundAction.Builder().add(new MouseMove(position), 1)
				.add(LMouseDown, 0).add(LMouseUp, 0).add(LMouseDown, 0).add(LMouseUp, 0).build();
	}

	public Action leftDoubleClickAt(double absX, double absY){
		return leftDoubleClickAt(new AbsolutePosition(absX, absY));
	}

	public Action leftDoubleClickAt(Widget w){
		return leftDoubleClickAt(w, 0.5, 0.5);
	}

	public Action leftDoubleClickAt(Widget w, double relX, double relY){
		return leftDoubleClickAt(new WidgetPosition(abstractor.apply(w), Tags.Shape, relX, relY, true));
	}

	public Action dragFromTo(Widget from, Widget to){
		return dragFromTo(from, 0.5, 0.5, to, 0.5, 0.5);
	}

	public Action dragFromTo(Widget from, double fromRelX, double fromRelY, Widget to, double toRelX, double toRelY){
		return dragFromTo(new WidgetPosition(abstractor.apply(from), Tags.Shape, fromRelX, fromRelY, true),
				new WidgetPosition(abstractor.apply(to), Tags.Shape, toRelX, toRelY, true));
	}

	public Action dragFromTo(Position from, Position to){
		return new CompoundAction.Builder().add(new MouseMove(from), 1)
				.add(LMouseDown, 0).add(new MouseMove(to), 1)
				.add(LMouseUp, 0).build();		
	}

	public Action clickTypeInto(final Position position, final String text){
		Assert.notNull(position, text);
		return new CompoundAction.Builder().add(leftClickAt(position), 1)
				.add(new Type(text), 1).build();
	}

	public Action clickTypeInto(Widget w, String text){
		return clickTypeInto(w, 0.5, 0.5, text);
	}

	public Action clickTypeInto(Widget w, double relX, double relY, String text){
		return clickTypeInto(new WidgetPosition(abstractor.apply(w), Tags.Shape, relX, relY, true), text);
	}
	
	public Action hitKey(KBKeys key){
		return new CompoundAction.Builder().add(new KeyDown(key), .0)
				.add(new KeyUp(KBKeys.VK_ESCAPE), 1).add(NOP, 1.0).build();
	}
	
	public Action killProcessByPID(long pid){ return killProcessByPID(pid, 0); }
	public Action killProcessByName(String name){ return killProcessByName(name, 0); }
	public Action killProcessByPID(long pid, double timeToWaitBeforeKilling){ return KillProcess.byPID(pid, timeToWaitBeforeKilling); }
	public Action killProcessByName(String name, double timeToWaitBeforeKilling){ return KillProcess.byName(name, timeToWaitBeforeKilling); }
	public Action activateSystem(){	return new ActivateSystem(); }
}
