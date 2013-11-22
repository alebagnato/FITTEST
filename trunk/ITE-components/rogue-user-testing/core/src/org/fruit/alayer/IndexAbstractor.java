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

import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import org.fruit.Util;

public final class IndexAbstractor implements Abstractor {

	private static final class IndexNode{
		int idx;
		IndexNode parent;
		public IndexNode(int idx, IndexNode parent){
			this.idx = idx;
			this.parent = parent;
		}
	}
	
	final boolean caching;
	Map<Widget, IndexNode> wtoi;
	
	public IndexAbstractor(){ this(true); }
	
	public IndexAbstractor(boolean enableCaching){
		caching = enableCaching;
		wtoi = new WeakHashMap<Widget, IndexNode>();
	}
		
	public void clearCache(){ wtoi.clear(); }
	public void cache(Widget root){ cache(root, null); }
	
	private void cache(Widget widget, IndexNode parentNode){		
		for(int i = 0; i < widget.childCount(); i++){
			Widget c = widget.child(i);
			IndexNode childNode = new IndexNode(i, parentNode);
			wtoi.put(c, childNode);
			cache(c, childNode);
		}			
	}
	
	private int[] getIndexPath(Widget widget){

		// caching
		if(caching){
			IndexNode node = wtoi.get(widget);
			if(node == null){
				cache(widget.root());
				node = wtoi.get(widget);
			}
			return indexArrayFromNode(node);
		}
		
		// no caching
		return Util.indexPath(widget);
	}
		
	private int[] indexArrayFromNode(IndexNode node){
		List<Integer> list = Util.newArrayList();
		
		while(node != null){
			list.add(node.idx);
			node = node.parent;
		}
		int size = list.size();
		int[] ret = new int[size];
		for(int i = 0; i < size; i++)
			ret[i] = list.get(size - i - 1);
		return ret;
	}
	
	public Finder apply(Widget widget) throws AbstractionException {
		if(widget.parent() == null)
			return new IndexFinder(new int[0]);
		return new IndexFinder(getIndexPath(widget));
	}
}
