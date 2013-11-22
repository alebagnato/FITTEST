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

import java.util.Collections;
import java.util.Set;
import org.fruit.Util;
import org.fruit.alayer.Rect;
import org.fruit.alayer.Tag;

public final class UIATags{
	private static final Set<Tag<?>> tagSet = Util.newHashSet();
	
	/**
	 * Type of a widget in localized form (language)
	 */
	public static final Tag<String> UIALocalizedControlType = from("UIALocalizedControlType", String.class);
	
	/**
	 * UIA Type name of a widget.
	 */
	public static final Tag<String> UIAItemType = from("UIAItemType", String.class);
	public static final Tag<String> UIAItemStatus = from("UIAItemStatus", String.class);
	public static final Tag<String> UIAProviderDescription = from("UIAProviderDescription", String.class);
	
	/**
	 * Win32 API ClassName of a widget
	 */
	public static final Tag<String> UIAClassName = from("UIAClassName", String.class);
	
	/**
	 * Help text of a widget
	 */
	public static final Tag<String> UIAHelpText = from("UIAHelpText", String.class);
	
	/**
	 * Title or name of a widget.
	 */
	public static final Tag<String> UIAName = from("UIAName", String.class);
	
	/**
	 * UIA Control Type Identifier of a widget
	 */
	public static final Tag<Long> UIAControlType = from("UIAControlType", Long.class);
	
	/**
	 * If the widget is a Win32 widget, this will be its native handle
	 */
	public static final Tag<Long> UIANativeWindowHandle = from("UIANativeWindowHandle", Long.class);
	public static final Tag<Long> UIAOrientation = from("UIAOrientation", Long.class);
	
	/**
	 * Id of the process that this widget belongs to
	 */
	public static final Tag<Long> UIAProcessId = from("UIAProcessId", Long.class);
	
	/**
	 * Id of the framework that this widget belongs to (e.g. Win32, Swing, Flash, ...)
	 */
	public static final Tag<String> UIAFrameworkId = from("UIAFrameworkId", String.class);
	public static final Tag<String> UIAAutomationId = from("UIAAutomationId", String.class);
	public static final Tag<long[]> UIARuntimeId = from("UIARuntimeId", long[].class);
	
	/**
	 * If the widget is a window, this tells whether it is the top-most window
	 */
	public static final Tag<Boolean> UIAIsTopmostWindow = from("UIAIsTopmostWindow", Boolean.class);

	/**
	 * If the widget is a window, this tells it is modal (such as a message box)
	 */
	public static final Tag<Boolean> UIAIsWindowModal = from("UIAIsWindowModal", Boolean.class);
	public static final Tag<Long> UIAWindowInteractionState = from("UIAWindowInteractionState", Long.class);
	public static final Tag<Long> UIAWindowVisualState = from("UIAWindowVisualState", Long.class);
	
	/**
	 * Bounding rectangle of a widget
	 */
	public static final Tag<Rect> UIABoundingRectangle = from("UIABoundingRectangle", Rect.class);
	
	/**
	 * Whether this widget is currently enabled or disabled (e.g. "greyed out")
	 */
	public static final Tag<Boolean> UIAIsEnabled = from("UIAIsEnabled", Boolean.class);
	
	/**
	 * Whether this widget has keyboard focus (i.e. will receive keyboard input)
	 */
	public static final Tag<Boolean> UIAHasKeyboardFocus = from("UIAHasKeyboardFocus", Boolean.class);
	
	/**
	 * Whether this widget is currently visible on the screen.
	 */
	public static final Tag<Boolean> UIAIsOffscreen = from("UIAIsOffscreen", Boolean.class);
	
	/**
	 * Whether this widget can be focused, such that it will receive keyboard input
	 */
	public static final Tag<Boolean> UIAIsKeyboardFocusable = from("UIAIsKeyboardFocusable", Boolean.class);

	private static <T> Tag<T> from(String name, Class<T> valueType){
		Tag<T> ret = Tag.from(name, valueType);
		tagSet.add(ret);
		return ret;
	}
	
	public static Set<Tag<?>> tagSet(){ return Collections.unmodifiableSet(tagSet); }
}
