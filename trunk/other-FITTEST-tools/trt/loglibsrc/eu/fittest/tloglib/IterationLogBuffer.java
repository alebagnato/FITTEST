package eu.fittest.tloglib;

import java.util.*;

/**
 * Class implementing the buffering to handle loops so that they do not
 * spam logging data (e.g. the tracing bits) when an iterarion does not
 * actually produce actual logging.
 * 
 * NOTE: this class has NOT been made ready to handle multi threads.
 * In multithreading setup, every thread should have its own iteration
 * buffer.
 */
public class IterationLogBuffer {
	
	// Just to wrap integers/longs :
	static public class Value {
		public Object val ;
		public Value(Object o) { val = o ; }
	}
	
	public List<StringBuffer> bufferstack ;  // to buffer the bits
	public List<StringBuffer> eventBufferstack ; // to buffer exception and untraced events
	public List<Value> loopIds ;
	public List<Value> prcs ; // the progress counter at the start of every iteration
	
	public IterationLogBuffer() {
		bufferstack = new LinkedList<StringBuffer>() ;
		eventBufferstack = new LinkedList<StringBuffer>() ;
		loopIds = new LinkedList<Value>() ;
		prcs = new LinkedList<Value>() ;
	}
	
	public boolean isEmpty() { return bufferstack.isEmpty() ; }
	
	/**
	 * Adding a fresh buffer to the bufferstack. It first clears the buffer, from the top,
	 * up to the first occurrence of id.
	 */
	public void addIteration(long id) {
		clearToId(id) ;
		bufferstack.add(new StringBuffer()) ;
		eventBufferstack.add(new StringBuffer()) ;
		loopIds.add(new Value(id)) ;
		prcs.add(new Value(TLog.getProgressCount())) ;
	}
	
	/**
	 * Append a bit-string to the top-buffer.
	 */
	public void appendTop(String s) {
		StringBuffer top = bufferstack.get(bufferstack.size() - 1) ;
		top.append(s) ;
	}
	
	/**
	 * Append a event-string to the top-buffer.
	 */
	public void appendEventTop(String s) {
		StringBuffer top = eventBufferstack.get(eventBufferstack.size() - 1) ;
		top.append(s) ;
	}
	
	private boolean containsLoopId(long id) {
		for (Value lid : loopIds) {
			if ((Long) lid.val == id)  return true ;
		}
		return false ;
	}
	
	/**
	 * Throw away all buffers from the top until (and including) the first occurrence of id.
	 * If however id does not occur, we do not clear.
	 */
	public void clearToId(long id) {
		//System.out.print("clearing iteration " + id + ", ") ;
		if (!containsLoopId(id)) return ;
		List<StringBuffer> toberemoved1 = new LinkedList<StringBuffer>() ;
		List<Value> toberemoved2 = new LinkedList<Value>() ;
		List<StringBuffer> toberemoved3 = new LinkedList<StringBuffer>() ;
		for (int i=bufferstack.size(); 0<i; i--) {
			int j = i-1 ;
			toberemoved1.add(bufferstack.get(j)) ;
			toberemoved2.add(loopIds.get(j)) ;
			toberemoved3.add(eventBufferstack.get(j)) ;
			TLog.setProgressCount((Integer) prcs.get(j).val) ;
			//System.out.print("//prc reset to " + TLog.prc + "//") ;
			prcs.remove(j) ;
			long id_ = (Long) loopIds.get(j).val ;
			if (id_ == id) break ;
		}
		bufferstack.removeAll(toberemoved1) ;
		loopIds.removeAll(toberemoved2) ;
		eventBufferstack.removeAll(toberemoved3) ;
	}
	
	public Pair<String,String> flush() {
		String z  = "" ;
		String ez = "" ;
		for (StringBuffer s : bufferstack) z += s ;
		for (StringBuffer s : eventBufferstack) ez += s ;		
		bufferstack.clear() ;
		eventBufferstack.clear() ;
		loopIds.clear() ;
		prcs.clear() ;
		return (new Pair(z,ez)) ;
	}

}
