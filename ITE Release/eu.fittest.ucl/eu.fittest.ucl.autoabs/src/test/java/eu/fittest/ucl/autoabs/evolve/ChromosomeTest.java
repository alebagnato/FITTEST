package eu.fittest.ucl.autoabs.evolve;

import static org.junit.Assert.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Random;
import java.util.Vector;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.cluster.ClusterAlgo;
import eu.fittest.ucl.autoabs.cluster.DistanceFunction;
import eu.fittest.ucl.autoabs.cluster.MinDistance;
import eu.fittest.ucl.autoabs.evolve.Chromosome;

import eu.fittest.ucl.autoabs.utils.TraceEvent;
import eu.fittest.ucl.autoabs.utils.Utils;

public class ChromosomeTest {

	private Random rnd;
	private ClusterAlgo clusterAlgo;	
	private Chromosome ind;
	private int numStates;
	private int numEvents;
	private File tmpCSV;
	private Vector<List<TraceEvent>> events;
	private int k;
	@Before
	public void setUp() {
		this.rnd = new Random(1);
		DistanceFunction dist = new MinDistance();
		this.clusterAlgo = new ClusterAlgo(dist);
		this.k = 3;
		
		/*START,0
		add,1
		add,2
		rem,1
		rem,0
		add,1
		pay,-1*/
		BufferedWriter bw = null;
		this.tmpCSV = new File("tmp.csv");
		this.numStates = 4;
		this.numEvents = 7;
		try {
			bw = new BufferedWriter(new FileWriter(this.tmpCSV));
			bw.write("event,n\n");
			bw.write("START,0\n");
			bw.write("add,1\n");
			bw.write("add,2\n");
			bw.write("rem,1\n");
			bw.write("rem,0\n");
			bw.write("add,1\n");
			bw.write("pay,-1\n");
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		Utils.clearConcreteStateSet();
		List<String> csvFiles = new ArrayList<String>();
		try {
			csvFiles.add(this.tmpCSV.getCanonicalPath());
		} catch (IOException e) {
			e.printStackTrace();
			assertFalse(true);
		}
		this.events = Utils.computeAllTraceEvents(csvFiles);
		this.clusterAlgo.agglomerate(Utils.getConcreteStates());
		this.ind = new Chromosome(this.rnd, this.clusterAlgo, Utils.getConcreteStates(), k);
	}

	@After
	public void tearDown() throws Exception {
		this.clusterAlgo = null;
		this.rnd = null;
		this.ind = null;
		if(this.tmpCSV != null) {
			this.tmpCSV.delete();
			this.tmpCSV = null;
		}
		this.events.clear();
		this.events = null;
	}
	
	@Test
	public void testConstruction() {
		//write a dummy csv trace file with duplicate states
		//check that utils only correct number of states
		assertEquals("unique states", numStates, Utils.getConcreteStates().size());
		//check that I have 1 list of correct number of events
		assertEquals("only 1 event trace", 1, events.size());
		List<TraceEvent> eventList = events.get(0);
		assertEquals("num events", numEvents, eventList.size());
		//assert I only have num states genes
		assertEquals("num genes", numStates, ind.getGenes().size());
		//assert I only have k cluster ids
		assertEquals("num clusters", k, ind.generateClusters().size());
		System.out.println(this.ind.toString());
	}
	@Test
	public void testDefaultMutate() {
		Set<Integer> origIds = new HashSet<Integer>();
		for(Cluster cluster : this.ind.generateClusters()) {
			origIds.add(cluster.getID());
		}
		
		Chromosome mutant = this.ind.cloneChromosome(true);
		mutant.mutate(0);
		
		//assert that I have 1 less cluster
		Set<Integer> muIds = new HashSet<Integer>();
		for(Cluster cluster : mutant.generateClusters()) {
			muIds.add(cluster.getID());
		}
		
		assertEquals("merge one state", origIds.size() - 1, muIds.size());
		
		mutant = this.ind.cloneChromosome(true);
		mutant.mutate(1);
		
		//assert i have 1 more state and two ids not in origIds
		muIds.clear();
		for(Cluster cluster : mutant.generateClusters()) {
			muIds.add(cluster.getID());
		}
		
		assertEquals("split one state", origIds.size() + 1, muIds.size());
		
		int numDiff = 0;
		for(Integer mid : muIds) {
			boolean found = false;
			for(Integer oid : origIds) {
				if(oid == mid) {
					found = true;
					break;
				}
			}
			if(!found)
				numDiff++;
		}
		for(Integer oid : origIds) {
			boolean found = false;
			for(Integer mid : muIds) {
				if(oid == mid) {
					found = true;
					break;
				}
			}
			if(!found)
				numDiff++;
		}
		assertEquals("two new ids", 3, numDiff); //2 new ids + 1 missing of the state that was split
		
		mutant = this.ind.cloneChromosome(true);
		mutant.mutate(2);
		//assert exactly 1 state is different
		assertEquals("same length", this.ind.getGenes().size(), mutant.getGenes().size());
		numDiff = 0;
		ArrayList<Gene> origGenes = this.ind.getGenes();
		ArrayList<Gene> muGenes = mutant.getGenes();
		for(int i = 0; i < origGenes.size(); i++) {
			if(origGenes.get(i).getClusterId() != muGenes.get(i).getClusterId())
				numDiff++;
		}
		assertEquals("1 mutated state", 1, numDiff);
	}
	
	@Test
	public void testDefaultCrossover() {		
		
		Chromosome[] chromosomes = new Chromosome[2];
		chromosomes[0] = new Chromosome(this.rnd, this.clusterAlgo, Utils.getConcreteStates(), -1);
		chromosomes[1] = new Chromosome(this.rnd, this.clusterAlgo, Utils.getConcreteStates(), -1);
		
		Chromosome orig = chromosomes[0].cloneChromosome(false);
		chromosomes[0].defaultCrossover(chromosomes[1]);
		
		
		//check that we have a new offspring
		boolean atLeastOneMismatch = false;
		outer:
		for(Cluster c1 : chromosomes[0].generateClusters()) {
			for(Cluster c2 : orig.generateClusters()) {
				if(c1.getID() != c2.getID()) {
					atLeastOneMismatch = true;
					break outer;
				}
			}
		}
		assertTrue("successful xover", atLeastOneMismatch);
	}
}
