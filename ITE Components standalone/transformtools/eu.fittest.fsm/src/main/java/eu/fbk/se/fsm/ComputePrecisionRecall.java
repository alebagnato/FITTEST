/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm;

import java.util.HashSet;
import java.util.Set;

public class ComputePrecisionRecall {
	
	static int countMarks(FSM fsm, String m) {
		Set<String> marks = new HashSet<String>();
		for (Node n : fsm.getNodes())
			for (Edge e : n.getSucc())
				for (String s : e.getMarks())
					if (s.startsWith(m))
						marks.add(s);
		return marks.size();
	}
	
	static void removeStartMarks(FSM fsm) {
		for (Edge e : fsm.getStartNode().getSucc())
			e.getMarks().clear();
	}
	
	static void computePrecisionRecall(String gsFile, String modFile) throws MaxFSMSizeExceededException {
		FSM gs = new FSM(gsFile);
		FSM mod = new FSM(modFile);
		gs.addAllMark("G");
		mod.addAllMark("M");
		FSMAlgo algo = new FSMAlgo();
		FSM u = algo.union(gs, mod);
		u = algo.makeDeterministic(u, 1000);
		u = algo.kTail(u, 2);
		u = algo.makeDeterministic(u, 1000);
		u.makeMarksUnique();
		int nG = countMarks(u, "G");
		int nM = countMarks(u, "M");
		gs.removeAllMarks();
		mod.removeAllMarks();
		FSM ip = algo.intersection(u, gs);
		ip = algo.makeDeterministic(ip, 1000);
		FSM ir = algo.intersection(u, mod);
		ir = algo.makeDeterministic(ir, 1000);
		int nIPM = countMarks(ip, "M");
		int nIRG = countMarks(ir, "G");
		double precision = (double)nIPM / (double)nM;
		double recall = (double)nIRG / (double)nG;
		gs.addAllMark("G");
		mod.addAllMark("M");
		gs.makeMarksUnique();
		mod.makeMarksUnique();
		int nIM = countMarks(mod, "M");
		int nIG = countMarks(gs, "G");
		FSM is = algo.intersection(gs, mod);
		int nISM = countMarks(is, "M");
		int nISG = countMarks(is, "G");		
		double intersection = (double)(nISM + nISG) / (double)(nIM + nIG);
		System.out.println("Precision    = " + nIPM + "/" + nM + " = " + precision);
		System.out.println("Recall       = " + nIRG + "/" + nG + " = " + recall);
		System.out.println("Intersection = " + (nISM + nISG) + "/" + (nIM + nIG) + " = " + intersection);
	}
	
	static public void main(String args[]) {
		if (args.length < 2) {
			System.err.println("Usage java eu.fbk.se.fsm.ComputePrecisionRecall gold-standard.fsm model.fsm");
			System.exit(1);
		}
		try {
			computePrecisionRecall(args[0], args[1]);
		} catch (MaxFSMSizeExceededException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
