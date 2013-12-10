package eu.fittest.ucl.eventinf.evolve;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Vector;

import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.FSMAlgo;
import eu.fbk.se.fsm.MaxFSMSizeExceededException;
import eu.fittest.ucl.eventinf.individual.Individual;

public class MutationOperator {
	
	enum MutationOperation {addTrace, mergeState, addSubtrace}
	
	private final int mutationProbability;
	private final Random ran;
	private final FSM[] basicFSM;
	private final Vector<List<String>> traces;
	private final FSMAlgo fsmAlgo = new FSMAlgo();
	
	private int MU_K_TAIL = 2;
	private int MAX_ADD_TRACE_ATTEMPTS = 100;
	private int MAX_FSM_SIZE = 10000;
	private int PROB_RANDOM_MERGE = 40;
	
	public MutationOperator(Random ran, FSM[] basicFSM, 
			Vector<List<String>> traces, int mutationProbability,
			int maxAddTraceAttempts, int maxFSMSize,
			int probRandomMerge)
	{
		this.MAX_ADD_TRACE_ATTEMPTS = maxAddTraceAttempts;
		this.MAX_FSM_SIZE = maxFSMSize;
		this.PROB_RANDOM_MERGE = probRandomMerge;
		
		this.ran = ran;
		this.basicFSM = basicFSM;
		this.traces = traces;
		this.mutationProbability = mutationProbability;
	}
	
	private FSM addTrace(FSM fsm) throws MaxFSMSizeExceededException {
		boolean added = false;
		int attempts = 0;
		while (!added && attempts < this.MAX_ADD_TRACE_ATTEMPTS) {
			attempts++;
			int j = ran.nextInt(basicFSM.length);
			if (!fsm.accepts(traces.get(j))) {
				added = true;
				fsm = fsmAlgo.union(fsm, basicFSM[j]);
				fsm = fsmAlgo.makeDeterministic(fsm, this.MAX_FSM_SIZE);
				fsm.regenerateLabels();
			}
		}
		//if (ran.nextInt(100) < GeneticAlgo.PROB_ADD_TRACE_REAPPLY)
		//	fsm = addTrace(fsm);
		return fsm;
	}
	
	private FSM mergeStates(FSM fsm) throws MaxFSMSizeExceededException {
		if (ran.nextInt(100) >= this.PROB_RANDOM_MERGE)
			fsm = fsmAlgo.randomKTail(fsm, MU_K_TAIL);
		else
			fsm = fsmAlgo.randomMerge(fsm);
		fsm = fsmAlgo.makeDeterministic(fsm, this.MAX_FSM_SIZE);
		fsm.regenerateLabels();
		//if (fsm.size() < GeneticAlgo.MAX_FSM_SIZE && ran.nextInt(100) < GeneticAlgo.PROB_K_TAIL_REAPPLY)
		//	fsm = mergeStates(fsm);
		return fsm;
	}
	
	/*
	private Node getStartNodeForSubtrace(FSM fsm, Set<String> tail, int k)
	{
		for(Node n : fsm.getNodes())
		{
			Set<String> nodeTail = new HashSet<String>();
			nodeTail.add("");		
			nodeTail = fsmAlgo.getTail(nodeTail, n, k);
			if(nodeTail.containsAll(tail))
			{
				return n;
			}
		}
		
		return null;
	}
	private FSM makeBasicFSM(List<String> events, 
			Node n, int k, FSM basicFSM)
	{
		if(k == 0) return basicFSM;
		
		String nextEvent = events.get(events.size() - k);
		
		for (Edge e : n.getSucc())
		{
			if(e.event.equals(nextEvent))
			{
				basicFSM.addNode(n.getLabel());
				basicFSM.getNode(n.getLabel()).addEdge(
						new Edge(e.getTarget(), e.event));
				
				return makeBasicFSM(
						events, 
						e.getTarget(), 
						k - 1, 
						basicFSM);
				
			}
		}
		
		return null;
	}
	
	private FSM addRandomSubtraceToFSM(FSM fsm, int k)
	{
		//get a random trace
		List<String> trace = traces.get(ran.nextInt(traces.size()));
		if(trace.size() <= 1) return fsm;
		//construct a random sub-trace
		int startIndex = ran.nextInt(trace.size() - 1);
		List<String> subTrace = trace.subList(startIndex, trace.size());
		Set<String> tail = new HashSet<String>();
		StringBuilder sb = new StringBuilder();
		for(String s : subTrace)
		{
			if(sb.length() != 0)
				sb.append(":" + s);
			else
				sb.append(s);
				
		}
		tail.add(sb.toString());
		
		Node startNode = getStartNodeForSubtrace(fsm, tail, k);
		
		if(startNode == null) return fsm;
		
		FSM basicFSM = new FSM();
		
		basicFSM = makeBasicFSM(subTrace,
				startNode, k, basicFSM);
		
		if(basicFSM == null) return fsm;
		
		basicFSM.setStartNode(basicFSM.getNode(startNode.getLabel()));
		
		//basicFSM.print();
		
		return fsmAlgo.union(fsm, basicFSM);
	}
	*/
	
	private void mutate(Individual ind, MutationOperation op)
	{
		FSM oldFSM = ind.getFSM();
		FSM fsm = ind.getFSM();
		switch(op)
		{
		case addTrace:
			try
			{
				fsm = addTrace(fsm);
			}
			catch(MaxFSMSizeExceededException e)
			{
				fsm = oldFSM;
			}
			break;
		case mergeState:
			try
			{
				fsm = mergeStates(fsm);
			}
			catch(MaxFSMSizeExceededException e)
			{
				fsm = oldFSM;
			}
			break;
		//case addSubtrace:
		//	fsm = addRandomSubtraceToFSM(fsm, 2);
		//	break;
		default:;
		}
		
		ind.setFSM(fsm);
	}
	
	public void mutate(ArrayList<Individual> offspringPopulation)
	{
		MutationOperation[] values = MutationOperation.values();
		
		for(int i = 0; i < offspringPopulation.size();++i)
		{
			if (ran.nextInt(100) < this.mutationProbability)
			{
				mutate(offspringPopulation.get(i), values[ran.nextInt(values.length)]);
			}
		}
	}
}
