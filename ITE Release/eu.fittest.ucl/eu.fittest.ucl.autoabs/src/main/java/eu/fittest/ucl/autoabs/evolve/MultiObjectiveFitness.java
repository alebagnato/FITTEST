package eu.fittest.ucl.autoabs.evolve;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import daikon.inv.Invariant;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.MeasureInfeas;
import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.evolve.Fitness.FitnessEvaluator;
import eu.fittest.ucl.autoabs.utils.TraceEvent;
import eu.fittest.ucl.autoabs.utils.Utils;

public class MultiObjectiveFitness extends Fitness {

	private final int maxInfCheckDepth;
	private final MeasureInfeas infChecker;
	
	public MultiObjectiveFitness(File outDir, Vector<List<TraceEvent>> events, int maxDepth, MeasureInfeas infChecker) {
		super(outDir, events);
		this.maxInfCheckDepth = maxDepth;
		this.infChecker = infChecker;
	}

	public int countInfeasibleSequences(FSM fsm) {
		if(this.infChecker == null) {
			//throw new RuntimeException("inf checker is null");
			return 0;
		} else
			return this.infChecker.measureInfeas(fsm, this.maxInfCheckDepth);
	}
	
	@Override
	public int compare(Chromosome o1, Chromosome o2) {
		int ndet1 = o1.getNDET();
		int ndet2 = o2.getNDET();
		int size1 = o1.getFSMSize();
		int size2 = o2.getFSMSize();
		int inf1 = o1.getInfeasible();
		int inf2 = o2.getInfeasible();
		
		if(ndet1 == ndet2 && size1 == size2 && inf1 == inf2) return 0;
		else if(ndet1 < ndet2 && size1 <= size2 && inf1 <= inf2) return -1;
		else if(ndet1 <= ndet2 && size1 < size2 && inf1 <= inf2) return -1;
		else if(ndet1 <= ndet2 && size1 <= size2 && inf1 < inf2) return -1;
		else if(ndet2 < ndet1 && size2 <= size1 && inf2 <= inf1) return 1;
		else if(ndet2 <= ndet1 && size2 < size1 && inf2 <= inf1) return 1;
		else if(ndet2 <= ndet1 && size2 <= size1 && inf2 < inf1) return 1;
		else return 0;
	}
	@Override
	public boolean areEqual(Chromosome o1, Chromosome o2) {
		int ndet1 = o1.getNDET();
		int ndet2 = o2.getNDET();
		int size1 = o1.getFSMSize();
		int size2 = o2.getFSMSize();
		int inf1 = o1.getInfeasible();
		int inf2 = o2.getInfeasible();
		
		if(ndet1 == ndet2 && size1 == size2 && inf1 == inf2) return true;
		return false;
	}
	
	@Override
	public void evaluate(Chromosome ind)
	{
		this.evaluate(ind, false);
	}
	
	@Override
	public void evaluate(Chromosome ind, boolean force) {
		/*Collection<Cluster> clusters = ind.generateClusters();
		int numClusters = clusters.size();
		if(numClusters == 1 && !force) {
			//penalize because we only have a single state (and start state)
			ind.setFSM(null);
			ind.setNDET(Integer.MAX_VALUE);
			ind.setInfeasible(Integer.MAX_VALUE);
			ind.setFSMSize(Integer.MAX_VALUE);
			return;
		}
		Collection<File> csvFilesForDaikon = Utils.saveClustersForDaikon(super.outDir, super.events, clusters);
		clusters.clear();
		clusters = null;
		
		Map<Integer, List<Invariant>> clusterInvMap = this.computeAllClusterInvariants(csvFilesForDaikon);
		
		//merge clusters that have the same invariants
		super.combineClustersWithIdenticalInv(ind, clusterInvMap);
		
		FSM fsm = super.buildFSM(ind, clusterInvMap);
		
		clusterInvMap.clear();
		clusterInvMap = null;
		
		*/
		
		FitnessEvaluator fitEvaluator = new FitnessEvaluator(this.events, this.outDir, ind, force); 
		Thread t = new Thread(fitEvaluator);
		t.start();
		try {
			t.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		FSM fsm = fitEvaluator.getFSM();
		fitEvaluator.dispose();
		fitEvaluator = null;
		t = null;
		
		if(fsm == null) {
			ind.setFSM(null);
			ind.setNDET(Integer.MAX_VALUE);
			ind.setInfeasible(Integer.MAX_VALUE);
			ind.setFSMSize(Integer.MAX_VALUE);
			return;
		}
		
		//merge nodes that share the same k-tail
		if(mergeTails)
			super.kTailMerge(fsm, ind);
		
		if(fsm.getNodes().size() == 1 && !force) {
			ind.setFSM(null);
			ind.setNDET(Integer.MAX_VALUE);
			ind.setInfeasible(Integer.MAX_VALUE);
			ind.setFSMSize(Integer.MAX_VALUE);
			return;
		}
		
		ind.setFSM(fsm);
		
		ind.setNDET(super.countNonDeterministicEvents(fsm));
		ind.setFSMSize(fsm.getNodes().size());
		ind.setInfeasible(this.countInfeasibleSequences(fsm)); 
				
		/*
		if(ind.getFSMSize() == 4) {
			System.out.println("AHAAAAA:" + ind.getNDET() + "," + ind.getInfeasible());
		} */
	}
}
