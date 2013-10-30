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

import org.fruit.Util;
import org.fruit.alayer.Color;
import org.fruit.alayer.EllipseVisualizer;
import org.fruit.alayer.FillPattern;
import org.fruit.alayer.Abstractor;
import org.fruit.alayer.Action;
import org.fruit.alayer.Pen;
import org.fruit.alayer.Position;
import org.fruit.alayer.Widget;
import org.fruit.alayer.OrthogonalPosition;
import org.fruit.alayer.StrokeCaps;
import org.fruit.alayer.Tags;
import org.fruit.alayer.TextVisualizer;
import org.fruit.alayer.TrajectoryVisualizer;
import org.fruit.alayer.WidgetPosition;
import org.fruit.alayer.devices.KBKeys;

public class AnnotatingActionCompiler extends StdActionCompiler {
	
	private static final Pen LClickPen = Pen.newPen().setColor(Color.Green)
			.setFillPattern(FillPattern.Solid).setStrokeWidth(3).build();
	private static final Pen RClickPen = Pen.newPen().setColor(Color.Yellow)
			.setFillPattern(FillPattern.None).setStrokeWidth(5).build();
	private static final Pen DoubleLClickPen = Pen.newPen().setColor(Color.Red)
			.setFillPattern(FillPattern.None).setStrokeWidth(5).build();
	private static final Pen DragDropPen = Pen.newPen().setColor(Color.CornflowerBlue)
			.setFillPattern(FillPattern.None).setStrokeWidth(2).setStrokeCaps(StrokeCaps._Arrow).build();
	private static final Pen TypePen = Pen.newPen().setColor(Color.Blue)
			.setFillPattern(FillPattern.None).setStrokeWidth(3).setFontSize(26).build();
	
	public AnnotatingActionCompiler(){ super(); }
	
	public AnnotatingActionCompiler(Abstractor abstractor){
		super(abstractor);
	}
	
	public Action leftClick(){
		Action ret = super.leftClick();
		ret.set(Tags.Desc, "Left Click");
		ret.set(Tags.Role, ActionRoles.LeftClick);
		return ret;
	}
	
	public Action rightClick(){
		Action ret = super.rightClick();
		ret.set(Tags.Desc, "Right Click");
		ret.set(Tags.Role, ActionRoles.RightClick);
		return ret;
	}

	public Action leftDoubleClick(){
		Action ret = super.leftDoubleClick();
		ret.set(Tags.Desc, "Left Double Click");
		ret.set(Tags.Role, ActionRoles.LDoubleClick);
		return ret;
	}

	public Action leftClickAt(Widget widget, double relX, double relY){
		Action ret = leftClickAt(new WidgetPosition(abstractor.apply(widget), Tags.Shape, relX, relY, true));
		ret.set(Tags.Desc, "Left Click at '" + widget.get(Tags.Desc, "<no description>") + "'");
		return ret;
	}
	
	public Action leftClickAt(Position position){
		Action ret = super.leftClickAt(position);
		ret.set(Tags.Desc, "Left Click at '" + position.toString() + "'");
		ret.set(Tags.Visualizer, new EllipseVisualizer(position, LClickPen, 10, 10));
		ret.set(Tags.Role, ActionRoles.LeftClickAt);
		return ret;
	}

	public Action rightClickAt(Position position){
		Action ret = super.rightClickAt(position);
		ret.set(Tags.Desc, "Right Click at '" + position.toString() + "'");
		ret.set(Tags.Visualizer, new EllipseVisualizer(position, RClickPen, 20, 20));
		ret.set(Tags.Role, ActionRoles.RightClickAt);
		return ret;
	}

	public Action leftDoubleClickAt(Position position){
		Action ret = super.leftDoubleClickAt(position);
		ret.set(Tags.Desc, "Double-Click at '" + position.toString() + "'");
		ret.set(Tags.Visualizer, new EllipseVisualizer(position, DoubleLClickPen, 30, 30));
		ret.set(Tags.Role, ActionRoles.LDoubleClickAt);
		return ret;
	}

	public Action clickTypeInto(final Widget widget, double relX, double relY, final String text){
		Action ret = clickTypeInto(new WidgetPosition(abstractor.apply(widget), Tags.Shape, relX, relY, true), text);
		ret.set(Tags.Desc, "Type '" + Util.abbreviate(text, 5, "...") + "' into '" + widget.get(Tags.Desc, "<no description>" + "'"));
		return ret;
	}

	public Action clickTypeInto(final Position position, final String text){
		Action ret = super.clickTypeInto(position, text);
		ret.set(Tags.Visualizer, new TextVisualizer(position, Util.abbreviate(text, 5, "..."), TypePen));
		ret.set(Tags.Desc, "Type '" + Util.abbreviate(text, 5, "...") + "' into '" + position.toString() + "'");
		ret.set(Tags.Role, ActionRoles.ClickTypeInto);		
		return ret;
	}
	
	public Action dragFromTo(Position from, Position to){
		Action ret = super.dragFromTo(from, to);
		ret.set(Tags.Visualizer, new TrajectoryVisualizer(DragDropPen, from, new OrthogonalPosition(from, to, 0.2, 0), to));
		ret.set(Tags.Desc, "Drag " + from.toString() + " To " + to.toString());
		ret.set(Tags.Role, ActionRoles.LeftDrag);		
		return ret;
	}
	
	public Action hitKey(KBKeys key){
		Action ret = super.hitKey(key);
		ret.set(Tags.Desc, "Hit Key " + key);
		ret.set(Tags.Role, ActionRoles.HitKey);		
		return ret;
	}
	
	
	public Action killProcessByPID(long pid, double timeToWaitBeforeKilling){
		Action ret = super.killProcessByPID(pid, timeToWaitBeforeKilling);
		ret.set(Tags.Desc, "Kill Process with pid: " + pid + ".");
		return ret;
	}
	
	public Action killProcessByName(String name, double timeToWaitBeforeKilling){
		Action ret = super.killProcessByName(name, timeToWaitBeforeKilling);
		ret.set(Tags.Desc, "Kill Process with name '" + name + "'");
		return ret;
	}
	
	public Action activateSystem(){	
		Action ret = super.activateSystem();
		ret.set(Tags.Desc, "Bring the system to the foreground.");
		return ret;
	}
}
