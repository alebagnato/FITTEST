package eu.fittest.ucl.eventinf.evolve;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Vector;

import java.util.Comparator;

import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.api.ModelInferenceListener;
import eu.fittest.ucl.eventinf.individual.Individual;
import eu.fittest.ucl.eventinf.individual.SimpleIndividual;
import eu.fittest.ucl.eventinf.reinsertion.ReinsertionStrategy;
import eu.fittest.ucl.eventinf.reinsertion.ReinsertionStrategy.Reinsertion;
import eu.fittest.ucl.eventinf.selection.Elitist;
import eu.fittest.ucl.eventinf.selection.SelectionOperator;
import eu.fittest.ucl.eventinf.selection.Tournament;

public class GeneticAlgo implements Comparator<Individual> {

	protected int populationSize;
	protected int offspringPopulationSize;
	protected int generations;
	protected boolean tournamentSelection;
	protected boolean useMinSize;
	protected int maxGenStringLength;
	protected int mutationProbability;
	protected int crossoverProbability;

	protected int maxFSMSize;
	protected int probRandomMerge;
	protected int maxAddTraceAttempts;

	protected final Random random;
	protected final ArrayList<ModelInferenceListener> listeners;

	FSM[] basicFSM;
	Vector<List<String>> traces;

	Individual[] population = new SimpleIndividual[populationSize];
	Vector<FSM> globallyNonDominated = new Vector<FSM>();

	MutationOperator muOp;
	CrossoverOperator crOp;
	SelectionOperator selectionOp;
	ReinsertionStrategy reinsertionOp;

	public GeneticAlgo(Random rand,
			ArrayList<ModelInferenceListener> listeners, FSM[] basicFSM) {
		this.listeners = listeners;
		this.random = rand;
		this.basicFSM = basicFSM;
		traces = new Vector<List<String>>();
		for (int i = 0; i < basicFSM.length; i++) {
			traces.add(basicFSM[i].serialPath());
		}
	}

	void initializePopulation() {
		muOp = new MutationOperator(this.random, basicFSM, traces,
				this.mutationProbability, this.maxAddTraceAttempts,
				this.maxFSMSize, this.probRandomMerge);

		crOp = new CrossoverOperator(this.random, offspringPopulationSize, false,
				this.crossoverProbability, this.maxFSMSize);

		reinsertionOp = new ReinsertionStrategy(this.random, Reinsertion.elitist);

		if (tournamentSelection)
			selectionOp = new Tournament(this.random, offspringPopulationSize);
		else
			selectionOp = new Elitist(offspringPopulationSize);
		
		for (int i = 0; i < populationSize; i++) {
			int j = this.random.nextInt(basicFSM.length);
			population[i] = new SimpleIndividual(basicFSM[j].clone(""));
		}
	}

	int unrecognizedTraces(FSM fsm) {
		int n = 0;
		for (int i = 0; i < traces.size(); i++)
			if (!fsm.accepts(traces.get(i)))
				n++;
		return n;
	}

	int unobservedStrings(FSM fsm) {
		Vector<List<String>> strings = fsm.generateStrings(maxGenStringLength);
		int n = 0;
		a: for (int i = 0; i < strings.size(); i++) {
			boolean observed = false;
			for (int j = 0; j < traces.size(); j++) {
				// List<String> trace = traces.get(j);
				List<String> trace = new LinkedList<String>();
				for (String ss : traces.get(j)) {
					trace.add(ss);
				}
				if (trace.size() < strings.get(i).size())
					continue;
				while (trace.size() > strings.get(i).size())
					trace.remove(trace.size() - 1);
				if (strings.get(i).equals(trace)) {
					observed = true;
					continue a;
				}
			}
			if (!observed)
				n++;
		}
		return n;
	}

	boolean dominates(FSM fsm1, FSM fsm2) {
		int unRec1 = unrecognizedTraces(fsm1);
		int unRec2 = unrecognizedTraces(fsm2);
		int unObs1 = unobservedStrings(fsm1);
		int unObs2 = unobservedStrings(fsm2);
		int size1 = 0;
		int size2 = 0;
		if (useMinSize) {
			size1 = fsm1.size();
			size2 = fsm2.size();
		}
		return (unRec1 < unRec2 && unObs1 <= unObs2 && size1 <= size2)
				|| (unRec1 <= unRec2 && unObs1 < unObs2 && size1 <= size2)
				|| (unRec1 <= unRec2 && unObs1 <= unObs2 && size1 < size2);
	}

	/*
	 * @Deprecated FSM tournament(FSM fsm1, FSM fsm2) { if (ran.nextInt(100) <
	 * PROB_SELECT_BEST) { if (fitness(fsm1) > fitness(fsm2)) return fsm1; else
	 * return fsm2; } else { if (fitness(fsm1) < fitness(fsm2)) return fsm1;
	 * else return fsm2; } }
	 */
	void fixGloballyNonDominated() {
		Vector<FSM> newGloballyNonDominated = new Vector<FSM>();
		for (int i = 0; i < globallyNonDominated.size(); i++) {
			boolean nonDominated = true;
			boolean equalFound = false;
			for (int j = 0; j < globallyNonDominated.size(); j++) {
				if (dominates(globallyNonDominated.get(j),
						globallyNonDominated.get(i)))
					nonDominated = false;
				if (i < j
						&& globallyNonDominated.get(i).equals(
								globallyNonDominated.get(j)))
					equalFound = true;
			}
			if (nonDominated && !equalFound) {
				newGloballyNonDominated.add(globallyNonDominated.get(i));
			}
		}
		globallyNonDominated = newGloballyNonDominated;
	}

	/*
	 * Individual[] select2(Individual[] pop) { int k = 0; Individual[] newPop =
	 * new SimpleIndividual[pop.length]; for (int i = 0 ; i < pop.length ; i++)
	 * { boolean nonDominated = true; for (int j = 0 ; j < pop.length ; j++) {
	 * if (dominates(pop[j].getFSM(), pop[i].getFSM())) nonDominated = false; }
	 * if (nonDominated) { newPop[k++] =
	 * pop[i].cloneIndividual(false);//.clone("");
	 * globallyNonDominated.add(pop[i].getFSM().clone("")); } }
	 * fixGloballyNonDominated(); while (k < pop.length) { int r =
	 * ran.nextInt(pop.length); newPop[k++] = pop[r].cloneIndividual(false); }
	 * return newPop; }
	 * 
	 * Individual[] select(Individual[] pop) { Individual[] newPop = new
	 * SimpleIndividual[pop.length]; int fit[] = new int[pop.length]; int
	 * totalFit = 0; for (int i = 0 ; i < pop.length ; i++) { fit[i] =
	 * (int)(1000 * pop[i].getFitness()); totalFit += fit[i]; } for (int i = 0 ;
	 * i < newPop.length ; i++) { boolean selected = false; while (!selected) {
	 * int r = ran.nextInt(totalFit); int cumFit = fit[0]; int j = 1; while (r >
	 * cumFit) cumFit += fit[j++]; if (pop[j-1].getFSM().size() < MAX_FSM_SIZE)
	 * { newPop[i] = pop[j-1].cloneIndividual(false);//.clone(""); selected =
	 * true; } } } return newPop; }
	 */
	Individual[] clone(Individual[] pop) {
		Individual[] newPop = new SimpleIndividual[pop.length];
		for (int i = 0; i < pop.length; i++)
			newPop[i] = pop[i].cloneIndividual(false);// .clone("");
		return newPop;
	}

	void printNonDominated() {
		for (int i = 0; i < globallyNonDominated.size(); i++) {
			System.out.println("========");
			System.out.println("UnObs = "
					+ unobservedStrings(globallyNonDominated.get(i)));
			System.out.println("UnRec = "
					+ unrecognizedTraces(globallyNonDominated.get(i)));
			System.out.println("Size  = " + globallyNonDominated.get(i).size());
			globallyNonDominated.get(i).print();
		}
	}

	private void updateNonDominated() {
		for (int i = 0; i < population.length; i++) {
			boolean nonDominated = true;
			for (int j = 0; j < population.length; j++) {
				if (dominates(population[j].getFSM(), population[i].getFSM()))
					nonDominated = false;
			}
			if (nonDominated) {
				globallyNonDominated.add(population[i].getFSM().clone(""));
			}
		}
		fixGloballyNonDominated();
	}

	public Vector<FSM> getParetoFront() {
		return this.globallyNonDominated;
	}

	/** Yue html result output */
	public void saveNonDominatedToFile(File path)

	{
		FSM curr_fsm = null;
		
		for (int i = 0; i < globallyNonDominated.size(); i++) {
			curr_fsm = globallyNonDominated.get(i);
			File model_dir = new File(path + File.separator + "model_"
					+ String.valueOf(i));
			model_dir.mkdir();
			File info = new File(model_dir + File.separator + "info.txt");
			try {
				// Create file
				FileWriter fstream = new FileWriter(info);
				BufferedWriter out = new BufferedWriter(fstream);
				out.write((unobservedStrings(curr_fsm)) + ","
						+ (unrecognizedTraces(curr_fsm)) + ","
						+ (curr_fsm.size()));

				// Close the output stream
				out.close();
			} catch (Exception e) {// Catch exception if any
				System.err.println("Error: " + e.getMessage());
			}
			try {
				/*
				 * FileOutputStream os = new FileOutputStream(model);
				 * PrintStream ps = new PrintStream(os); curr_fsm.print(ps);
				 * os.flush(); os.close(); ps.flush(); ps.close();
				 */
				curr_fsm.print(model_dir + File.separator + "fsm.txt"); // new
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return;
			} catch (Exception e) {
				e.printStackTrace();
				return;
			}
		}
	}

	protected void reportProgress(double p, String msg) {
		String message = msg == null ? "Evolving models" : msg;
		for (ModelInferenceListener listener : this.listeners) {
			listener.progress(p, message);
		}
	}

	public void run() {
		initializePopulation();
		for (int k = 0; k < generations; k++) {

			this.reportProgress((k * 100) / generations, null);

			ArrayList<Individual> selectedInd = this.selectionOp.getParents(
					population, this);// select2(population);

			ArrayList<Individual> offspringPopulation = this.crOp
					.crossover(selectedInd);

			this.muOp.mutate(offspringPopulation);

			population = reinsertionOp.updatePopulation(population,
					selectedInd, this);

			updateNonDominated();
		}
	}

	/*
	 * void runAlgo() { initializePopulation(); for (int k = 0 ; k < GENERATIONS
	 * ; k++) { int i = 0; Individual[] selectedInd; if (!TOURNAMENT)
	 * selectedInd = select(population); else selectedInd = clone(population);
	 * 
	 * for (Individual ind : selectedInd) { FSM oldFsm = ind.getFSM();
	 * muOp.mutate((Individual)ind); if (TOURNAMENT)
	 * ind.setFSM(tournament(oldFsm, ind.getFSM())); population[i++] = ind; } }
	 * }
	 */

	public int compare(Individual arg0, Individual arg1) {
		// TODO Auto-generated method stub
		FSM fsm1 = arg0.getFSM();
		FSM fsm2 = arg1.getFSM();
		if (dominates(fsm1, fsm2))
			return -1;
		else if (dominates(fsm2, fsm1))
			return 1;
		else
			return 0;
	}

	// -------
	void delete(File f) throws IOException {
		if (f.isDirectory()) {
			for (File c : f.listFiles())
				delete(c);
		}
		if (!f.delete())
			throw new FileNotFoundException("Failed to delete file: " + f);
	}

	public void setPopulationSize(int populationSize) {
		this.populationSize = populationSize;
	}

	public void setOffspringPopulationSize(int offspringPopulationSize) {
		this.offspringPopulationSize = offspringPopulationSize;
	}

	public void setGenerations(int generations) {
		this.generations = generations;
	}

	public void setTournamentSelection(boolean tournamentSelection) {
		this.tournamentSelection = tournamentSelection;
	}

	public void setUseMinSize(boolean useMinSize) {
		this.useMinSize = useMinSize;
	}

	public void setMaxGenStringLength(int maxGenStringLength) {
		this.maxGenStringLength = maxGenStringLength;
	}

	public void setMuOp(MutationOperator muOp) {
		this.muOp = muOp;
	}

	public void setMutationProbability(int mutationProbability) {
		this.mutationProbability = mutationProbability;
	}

	public void setCrossoverProbability(int crossoverProbability) {
		this.crossoverProbability = crossoverProbability;
	}

	public void setMaxFSMSize(int maxFSMSize) {
		this.maxFSMSize = maxFSMSize;
	}

	public void setProbRandomMerge(int probRandomMerge) {
		this.probRandomMerge = probRandomMerge;
	}

	public void setMaxAddTraceAttempts(int maxAddTraceAttempts) {
		this.maxAddTraceAttempts = maxAddTraceAttempts;
	}

}

