/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm.utils;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import eu.fbk.se.fsm.cte.Class;
import eu.fbk.se.fsm.cte.Classification;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.CteObject;


public class CteUtils {
	
	public static  HashMap<String, Vector<String>> extractMapNameVectorID(CteObject cteTree){
		Composition testSequence = null;
		
		HashMap<String, Vector<String>> map = new HashMap<String, Vector<String>>();
		
		List<Object> oList = ((Composition) cteTree.getTree().getRoot())
				.getCompositionOrClassification();
		if (!oList.isEmpty()) {
			for (Object o : oList) {
				if (o instanceof Composition) {
					testSequence = (Composition) o;
					break; // first composition found is considered as test  sequence,
					// this list should have only one element
				}
			}
		}
		
		if (testSequence != null){
			List<Object> events = testSequence.getCompositionOrClassification();
			for (Object o : events){
				if (o instanceof Composition){
					if (!((Composition) o).getCompositionOrClassification().isEmpty()){
						Object firstClassification = ((Composition) o).getCompositionOrClassification().get(0);
						if (firstClassification instanceof Classification){
							Classification classification = (Classification) firstClassification;
							
							List<Class> lst = classification.getClazz();
							for (Class clz : lst){
								Vector<String> idVector = map.get(clz.getName());
								if (idVector == null){
									idVector = new Vector<String>();
									map.put(clz.getName(), idVector);
								}
								idVector.add(clz.getId());
										
							}
						}
					}
				}
			}
		}
		
		return map;
	}
	
	public static HashMap<String, String> extractMapNameId(CteObject cteTree) {
		Composition testSequence = null;
		HashMap<String, String> map = new HashMap<String, String>();
		List<Object> oList = ((Composition) cteTree.getTree().getRoot())
				.getCompositionOrClassification();
		if (!oList.isEmpty()) {
			for (Object o : oList) {
				if (o instanceof Composition) {
					testSequence = (Composition) o;
					break; // first composition found is considered as test  sequence,
					// this list should have only one element
				}
			}
		}
		
		if (testSequence != null){
			List<Object> events = testSequence.getCompositionOrClassification();
			for (Object o : events){
				if (o instanceof Composition){
					if (!((Composition) o).getCompositionOrClassification().isEmpty()){
						Object firstClassification = ((Composition) o).getCompositionOrClassification().get(0);
						if (firstClassification instanceof Classification){
							Classification classification = (Classification) firstClassification;
							List<Class> listClz =  classification.getClazz();
							
							for (Class clz : listClz){
								map.put(clz.getName(), clz.getId());
							}
						}
					}
				}
			}
		}

		
		return map;
	}

	
	public static HashMap<String, String> extractMapIdName(CteObject cteTree) {
		Composition testSequence = null;
		HashMap<String, String> map = new HashMap<String, String>();
		List<Object> oList = ((Composition) cteTree.getTree().getRoot())
				.getCompositionOrClassification();
		if (!oList.isEmpty()) {
			for (Object o : oList) {
				if (o instanceof Composition) {
					testSequence = (Composition) o;
					break; // first composition found is considered as test  sequence,
					// this list should have only one element
				}
			}
		}
		
		if (testSequence != null){
			List<Object> events = testSequence.getCompositionOrClassification();
			for (Object o : events){
				if (o instanceof Composition){
					if (!((Composition) o).getCompositionOrClassification().isEmpty()){
						Object firstClassification = ((Composition) o).getCompositionOrClassification().get(0);
						if (firstClassification instanceof Classification){
							Classification classification = (Classification) firstClassification;
							List<Class> listClz =  classification.getClazz();
							
							for (Class clz : listClz){
								map.put(clz.getId(), clz.getName());
							}
						}
					}
				}
			}
		}
		
		
		return map;
	}
	
	
	public static int getSequenceSize(CteObject cte){
		Composition testSequence = null;
		List<Object> oList = ((Composition) cte.getTree().getRoot())
				.getCompositionOrClassification();
		if (!oList.isEmpty()) {
			for (Object o : oList) {
				if (o instanceof Composition) {
					testSequence = (Composition) o;
					break; // first composition found is considered as test  sequence,
					// this list should have only one element
				}
			}
		}
		
		if (testSequence != null){
			List<Object> events = testSequence.getCompositionOrClassification();
			return events.size();
		}
		
		return 0;
	}
	
}
