/*
  Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya
  
  Copyright 2011 Utrecht University.
  
  The use of this sofware is free under the Modified BSD License. 

*/

package eu.fittest.Logging.Serialization {

	/**
	 * The logger can serialize the state of an object, but it needs to know 
	 * how it should do this, e.g. which fields are to be included. One way
	 * to specify this is by making the object (or rather, the class to which
	 * it belongs) to implement this interface. The logger will then automatically
	 * use the function serializeSelf from this interface.
	 */ 
    public interface Serializable {
    
        /**
		 * Specify in this function, how to serialize the target object. E.g. which
		 * fields are to be serialized, and how exactly each should be serialized.
		 */ 
        function serializeSelf(serializer : Serializer) : void;
    
        //var serializer:Serializer;
        //function serializeSelf() : void;
    }

}
