package eu.fittest.ucl.autoabs.evolve;

import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

/** This class represents a chromosome gene
 * 
 * @author kiranlakhotia
 *
 */
public class Gene {
	
	//The concretestate
	private final ConcreteState state;
	
	//The cluster id to which this gene belongs
	private int clusterId;
	
	public Gene(ConcreteState state, int clusterId) {
		this.state = state;
		this.clusterId = clusterId;
	}
	public Gene(Gene seed) {
		this.state = seed.state;
		this.clusterId = seed.clusterId;
	}
	public ConcreteState getConcreteState() {
		return this.state;
	}
	public void setClusterId(int id) {
		this.clusterId = id;
	}
	public int getClusterId() {
		return this.clusterId;
	}
}
