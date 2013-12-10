package eu.fittest.ucl.eventinf.api;

public class ModelInferenceParameters {
	
	private int populationSize = 1000; //1000
	private int offspringPopulationSize = 500; //500
	private int maxGenerations = 100; //100
	//static int PROB_ADD_TRACE = 30;
	//static int PROB_SELECT_BEST = 70;
	//private int kTail = 8;
	//private int probKTailReapply = 60;
	//static int PROB_ADD_TRACE_REAPPLY = 10;
	private int probRandomMerge = 10;
	private boolean tournamentSelection = true;
	private boolean useMinSize = true;
	private int maxGenStringLength = 8;
	private int maxFSMSize = 10000;
	private int maxAddTraceAttempts = 1000;
		
	private int probCrossover = 50;
	
	private boolean sampleWithReplacement = false;
	
	private int probMutation = 50;
	private int mutationKTail = 2;
	public int getPopulationSize() {
		return populationSize;
	}
	public void setPopulationSize(int populationSize) {
		this.populationSize = populationSize;
	}
	public int getOffspringPopulationSize() {
		return offspringPopulationSize;
	}
	public void setOffspringPopulationSize(int offspringPopulationSize) {
		this.offspringPopulationSize = offspringPopulationSize;
	}
	public int getMaxGenerations() {
		return maxGenerations;
	}
	public void setMaxGenerations(int maxGenerations) {
		this.maxGenerations = maxGenerations;
	}
	/*public int getkTail() {
		return kTail;
	}
	public void setkTail(int kTail) {
		this.kTail = kTail;
	}
	public int getProbKTailReapply() {
		return probKTailReapply;
	}
	public void setProbKTailReapply(int probKTailReapply) {
		this.probKTailReapply = probKTailReapply;
	}
	public int getProbRandomMerge() {
		return probRandomMerge;
	}
	public void setProbRandomMerge(int probRandomMerge) {
		this.probRandomMerge = probRandomMerge;
	}*/
	public boolean isTournamentSelection() {
		return tournamentSelection;
	}
	public void setTournamentSelection(boolean tournamentSelection) {
		this.tournamentSelection = tournamentSelection;
	}
	public boolean isUseMinSize() {
		return useMinSize;
	}
	public void setUseMinSize(boolean useMinSize) {
		this.useMinSize = useMinSize;
	}
	public int getMaxGenStringLength() {
		return maxGenStringLength;
	}
	public void setMaxGenStringLength(int maxGenStringLength) {
		this.maxGenStringLength = maxGenStringLength;
	}
	public int getMaxFSMSize() {
		return maxFSMSize;
	}
	public void setMaxFSMSize(int maxFSMSize) {
		this.maxFSMSize = maxFSMSize;
	}
	public int getMaxAddTraceAttempts() {
		return maxAddTraceAttempts;
	}
	public void setMaxAddTraceAttempts(int maxAddTraceAttempts) {
		this.maxAddTraceAttempts = maxAddTraceAttempts;
	}
	public int getProbCrossover() {
		return probCrossover;
	}
	public void setProbCrossover(int probCrossover) {
		this.probCrossover = probCrossover;
	}
	public boolean isSampleWithReplacement() {
		return sampleWithReplacement;
	}
	public void setSampleWithReplacement(boolean sampleWithReplacement) {
		this.sampleWithReplacement = sampleWithReplacement;
	}
	public int getProbMutation() {
		return probMutation;
	}
	public void setProbMutation(int probMutation) {
		this.probMutation = probMutation;
	}
	public int getMutationKTail() {
		return mutationKTail;
	}
	public void setMutationKTail(int mutationKTail) {
		this.mutationKTail = mutationKTail;
	}
	public int getProbRandomMerge() {
		return probRandomMerge;
	}
	public void setProbRandomMerge(int probRandomMerge) {
		this.probRandomMerge = probRandomMerge;
	}
}
