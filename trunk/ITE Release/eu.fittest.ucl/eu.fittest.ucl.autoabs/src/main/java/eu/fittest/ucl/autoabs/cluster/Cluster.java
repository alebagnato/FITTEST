/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.cluster;

import java.util.LinkedHashSet;
import java.util.List;

import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;
import eu.fittest.ucl.autoabs.utils.Utils;

/** 
 * Cluster of concrete states
 */
public class Cluster {
	
	/** unique ID counter */
	private static int ID = -1;
	
	private final int clusterId;
	
	/** initial states belonging to the cluster */
	private LinkedHashSet<ConcreteState> states;
	
	private Cluster parent;
	
	public Cluster()
	{
		this(++ID);
	}
	public Cluster(int id) {
		this.clusterId = id;
		this.states = new LinkedHashSet<ConcreteState>();
		this.parent = null;
	}
	public Cluster(List<ConcreteState> states)
	{
		this(++ID);
		this.states.addAll(states);
	}
	public Cluster(ConcreteState state, Cluster parent)
	{
		this(++ID);
		this.states.add(state);
		this.parent = parent;
	}
	public Cluster(Cluster c1, Cluster c2, Cluster parent)
	{
		this(++ID);
		this.states.addAll(c1.states);
		this.states.addAll(c2.states);
		this.parent = parent;
	}
	public Cluster(Cluster c1, Cluster c2)
	{
		this(c1, c2, null);
	}
	/** set dendrogram ancestor */
	public void setParent(Cluster c)
	{
		this.parent = c;
	}
	public Cluster getParent(){
		return this.parent;
	}
	public boolean containsState(ConcreteState state)
	{
		//return this.states.contains(state);
		for(ConcreteState cs : this.states) {
			if(Utils.areEqualStates(cs, state))
				return true;
		}
		return false;
	}
	public int getID()
	{
		return this.clusterId;
	}
	public void addStates(List<ConcreteState> states) {
		this.states.addAll(states);
	}
	public void addState(ConcreteState state) {
		this.states.add(state);
	}
	public LinkedHashSet<ConcreteState> getStates(){
		return this.states;
	}
	public int getNumStates() {
		return this.states.size();
	}
	public void dispose() {
		this.states.clear();
	}
	@Override
	public String toString(){
		StringBuilder sb = new StringBuilder(this.states.size());
		boolean prependSep = false;
		for(ConcreteState state : this.states) {
			if(prependSep){
				sb.append("|");
			}
			sb.append(state.toString());
			prependSep = true;
		}
		return sb.toString();
	}
}
