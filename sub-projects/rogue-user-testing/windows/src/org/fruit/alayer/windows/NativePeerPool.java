/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.alayer.windows;

public final class NativePeerPool {

	public enum PeerType{ IUnknown; }

	public static final NativePeer InvalidPeer = new InvalidPeer();
	
	public interface NativePeer{
		long access();
		void release();
		boolean valid();
	}
	
	private final static class InvalidPeer implements NativePeer{
		private InvalidPeer(){};
		public long access() { throw new IllegalStateException(); }
		public void release() { }
		public boolean valid() { return false; }	
	}
	
	public final class StdPeer implements NativePeer{
		private long ptr;
		private StdPeer pred, succ;
		private final PeerType type;

		private StdPeer(StdPeer predecessor, StdPeer successor, long ptr, PeerType type){ 
			this.ptr = ptr;
			succ = successor;
			pred = predecessor;
			this.type = type;
		}
		
		public long access() throws IllegalStateException {
			if(ptr == 0)
				throw new IllegalStateException("Peer has already been released!");
			return ptr;
		}

		public boolean valid(){ return ptr != 0; }

		public void release(){
			if(ptr == 0)
				return;

			switch(type){
			case IUnknown: Windows.IUnknown_Release(ptr);
			}
			ptr = 0;
			
			if(pred != null)
				pred.succ = succ;
			if(succ != null)
				succ.pred = pred;
			if(this == NativePeerPool.this.first)	// we've been the first one, so make the second the new first
				NativePeerPool.this.first = succ;
		}
	}

	private StdPeer first = null;

	public NativePeer register(long ptr, PeerType type){
		if(ptr == 0)
			return InvalidPeer;
		
		StdPeer ret = new StdPeer(null, first, ptr, type);
		if(first != null)
			first.pred = ret;
		first = ret;
		return ret;
	}

	public void release(){
		while(first != null)
			first.release();
	}

	public void finalize(){ release(); }
}
