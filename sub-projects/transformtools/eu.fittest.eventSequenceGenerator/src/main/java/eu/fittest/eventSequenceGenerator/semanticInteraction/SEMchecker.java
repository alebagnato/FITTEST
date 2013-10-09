/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.semanticInteraction;

import java.util.List;
import java.util.LinkedList;

import eu.fittest.eventSequenceGenerator.data.Edge;
import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;

/**
*
* @author Alessandro
*
*/
public class SEMchecker {

	public static boolean isSEM(FSM fsm, Edge e1, Edge e2, Node s0, Node s1){
		Node s2=null;
		//System.out.println("try1");
		//System.out.print("s0="+s0.getLabel()+" e1="+e1.getEvent()+" (s0_1="+e1.getTarget().getLabel()+") e2="+e2.getEvent()+" s1="+s1.getLabel());
		
		try {
			//System.out.println("try2:"+fsm.getStartNode().getLabel());
		List<Edge> outEdges=fsm.getNodeByLabel(s0.getLabel()).getSucc();
		for (Edge edge : outEdges) {
			
			//System.out.println("2c.2");
			if (edge.getEvent().equals(e2.getEvent())){
				//System.out.println("2c.3");
				
				List<Edge> outEdges2=edge.getTarget().getSucc();
				for (Edge edge2 : outEdges2) {
					//System.out.println("2c.4");
					if (edge2.getEvent().equals(e1.getEvent())){
						s2=edge2.getTarget();
						//System.out.println("2c.5");
						//uguale o diverso ? dovrebbe essere diverso
						
						if (!s2.getLabel().equals(s1.getLabel())) return true;
						
						}
					}
			}
			
		}
		return false;
		}catch (Exception e){return false;}
	}
	
	
	public static boolean isALTSEM(FSM fsm, List<Edge> e1s, Edge e2, Node s0, Node s1){
		Node s2=null;
		List<Edge> outedge2=null;
		
		List<Edge> outEdgesOfs0=fsm.getNodeByLabel(s0.getLabel()).getSucc();
		for (Edge outedge_s0 : outEdgesOfs0) {
			
			if (outedge_s0.getEvent().equals(e2.getEvent())){
				outedge2=outedge_s0.getTarget().getSucc();
				break;
			}
		}
		
		if (outedge2==null) return false;
		
		for (int j = 0; j < e1s.size(); j++) {
			if(j==e1s.size()-1){
					s2=e1s.get(j).getTarget();
					if (!s2.getLabel().equals(s1.getLabel())) return true;
					else return false;
			}
			else {
				if (existsInOutEdges(outedge2,e1s.get(j))){			
						outedge2=e1s.get(j).getTarget().getSucc();
				}else break;
			}
		}
		
		return false;
	}
	
	static boolean existsInOutEdges(List<Edge> outEdges, Edge eToBeChecked){
		for (Edge edge : outEdges) {
			if (edge.getEvent().equals(eToBeChecked.getEvent())){
				return true;
			}
		}
		return false;
	}
	
}
