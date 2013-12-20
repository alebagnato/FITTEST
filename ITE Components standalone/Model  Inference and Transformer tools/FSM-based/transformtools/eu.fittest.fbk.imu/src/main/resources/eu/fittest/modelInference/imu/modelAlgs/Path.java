package eu.fittest.modelInference.imu.modelAlgs;
import java.util.Vector;


import eu.fittest.modelInference.fsmInference.fsm.Transition;

/**
 * 
 * 
 * @author Alessandro Marchetto
 *
 */
public class Path {

		private static int pathNumber = 0;
		
		private int id; //id unique of the path
		private Vector<Long> pathStatesVector; //vettore degli id degli stati della FSM
		private Vector<Long> pathTransitionsVector; //vettore degli id delle transizioni della FSM
		
		/**
		 * @param id
		 * @param pathStatesVector
		 * @param pathProbability
		 */
		public Path() {
			super();
			this.id = pathNumber++;
			this.pathStatesVector = new Vector<Long>();		
			this.pathTransitionsVector = new Vector<Long>();		
		}
		
		public Path(Vector<Long> pathVector, Vector<Long> pathTransitionsVector) {
			super();
			this.id = pathNumber++;
			this.pathStatesVector = pathVector;
			this.pathTransitionsVector=pathTransitionsVector;
		}
		
		
		@Override
		public boolean equals (Object other){
			if (other==null) return false;
			if (((Path)other).getPathVector().size() != this.pathStatesVector.size()){
				return false;
			}else if (((Path)other).getPathTransitionsVector().size() != this.pathTransitionsVector.size()){
				return false;
			}
			else {
				if (!isequalsStates(((Path)other).getPathVector())) return false;	
				if (!isequalsTransitions(((Path)other).getPathTransitionsVector())) return false;
			}
			return true;
		}
		
		boolean isequalsStates(Vector<Long> states){
			if (states==null) return false;
			Long str;
			boolean eq=false;
			for (int i = 0; i < this.pathStatesVector.size(); i++) {
				str=this.pathStatesVector.get(i);
				 if (str.longValue()==states.get(i).longValue()) eq=true;
				 else return false;
			}
			return eq;
		}
		
		public boolean iscontainedStates(Long state){
			Long str;
			
			for (int i = 0; i < this.pathStatesVector.size(); i++) {
				str=this.pathStatesVector.get(i);
				 if (str.longValue()==state.longValue()) return true;
			}
			return false;
		}
		
		boolean isequalsTransitions(Vector<Long> transitionV){
			if (transitionV==null) return false;
			Long str;
			boolean eq=false;
			for (int i = 0; i < this.pathTransitionsVector.size(); i++) {
				str=this.pathTransitionsVector.get(i);
				 if (str.longValue()==transitionV.get(i).longValue()) eq=true;
				 else return false;
			}
			return eq;
		}
		
		public boolean iscontainedTransition(Long transition){
			Long str;

			for (int i = 0; i < this.pathTransitionsVector.size(); i++) {
				str=this.pathTransitionsVector.get(i);
				if (str.longValue()==transition.longValue()) return true;
			}
			return false;
		}
		
		
		/**
		 * @return the id
		 */
		public int getId() {
			return id;
		}
		/**
		 * @param id the id to set
		 */
		public void setId(int id) {
			this.id = id;
		}
		/**
		 * @return the pathVector
		 */
		public Vector<Long> getPathVector() {
			return pathStatesVector;
		}
		/**
		 * @param pathVector the pathVector to set
		 */
		public void setPathVector(Vector<Long> pathVector) {
			this.pathStatesVector = pathVector;
		}
		
		public void setPathTransitionsVector(Vector<Long> pathTransitionsVector) {
			this.pathTransitionsVector = pathTransitionsVector;
		}
		public Vector<Long> getPathTransitionsVector() {
			return pathTransitionsVector;
		}
		
		public int size(){
			return pathTransitionsVector.size();
		}
	
}
