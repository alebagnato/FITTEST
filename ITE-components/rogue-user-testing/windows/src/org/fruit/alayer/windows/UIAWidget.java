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
import java.util.ArrayList;
import java.util.Map;
import org.fruit.Util;
import org.fruit.alayer.Tag;
import org.fruit.alayer.Widget;

class UIAWidget implements Widget, Serializable {
	private static final long serialVersionUID = 8840515358018797073L;
	UIAState root;
	UIAWidget parent;
	Map<Tag<?>, Object> tags = Util.newHashMap();
	ArrayList<UIAWidget> children = new ArrayList<UIAWidget>();
	UIAElement element;
		
	protected UIAWidget(UIAState root, UIAWidget parent, UIAElement element){
		this.parent = parent;
		this.element = element;
		this.root = root;
		
		if(parent != null)
			root.connect(parent, this);
	}
	
	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException{
		ois.defaultReadObject();
	}
	
	private void writeObject(ObjectOutputStream oos) throws IOException{
		oos.defaultWriteObject();
	}

	
	final boolean valid(){ return root != null; }
	final void check(){ if(root == null) throw new IllegalStateException(); }
	
	final public void moveTo(Widget p, int idx) { /*check();*/ root.setParent(this, p, idx); }
	public final UIAWidget addChild() { /*check();*/ return root.addChild(this, null); }
	public final UIAState root() { return root; }
	public final UIAWidget parent() { /*check();*/ return root.getParent(this); }
	public final UIAWidget child(int i) { /*check();*/ return root.getChild(this, i); }
	public final void remove() { /*check();*/ root.remove(this); }
	public final int childCount() { /*check();*/ return root.childCount(this); }

	public final <T> T get(Tag<T> t) { /*check;*/ return root.get(this, t); }
	public final <T> void set(Tag<T> tag, T value) { /*check;*/ root.setTag(this, tag, value); }
	public final <T> T get(Tag<T> tag, T defaultValue) { /*check;*/ return root.get(this, tag, defaultValue); }
	public final Iterable<Tag<?>> tags() { /*check;*/ return root.tags(this); }
	public final void remove(Tag<?> tag) { /*check;*/ root.remove(this, tag); }			
}
