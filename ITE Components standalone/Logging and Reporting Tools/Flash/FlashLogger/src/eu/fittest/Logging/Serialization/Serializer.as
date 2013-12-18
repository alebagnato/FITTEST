/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging.Serialization {	

	/**
	 * This defines an interface for a so-called serializer. A serializer provides a set
	 * of methods to specify how an object is to be serialized; e.g. which fields are
	 * to be serialized, and how each field is to be serialized (converted to string).
	 * E.g. when a field contains an array, maybe we just want to serialize its length,
	 * rather than the whole content of the array. Given a specification on how to serialze
	 * an object, the serializer takes over the task of actually glueing and formatting 
	 * the produced strings together.
	 */
	public interface Serializer {
	
        /**
		 * This function tells the serializer that we now start with 
		 * serializing an object.
		 * 
		 * <p>The object should not be null nor undefined when this 
		 * method is called.</p>
		 * 
		 * @param val the object to serialize
		 * 
		 * @param objType a string which is the object's type; the string is 
		 * wrapped as an object to avoid being passed by value.
		 */
		function beginObject(val:Object , objType : Object) : void;
		
		/**
		 * This function tells the serializer that we are done with serializing 
		 * an object. Calls to beginObject and endObject may be nested, but 
		 * must be balanced.
		 */ 
		function endObject() : void;
		
		/**
		 * This function tells the serializer to serialize/store the given 
		 * field/attribute of the object that belongs to the latest 'beginObject'.
		 * 
		 * @param name a string which is the name of the field; it is wrapped as 
		 * an object to avoid pass by value.
		 * 
		 * @param val the value of the field.
		 */ 
		function storeField(name:Object, val:*) : void;
		
		/**
		 * This function should reset the internal state of the serializer.
		 * Call this function before start serializing a new object structure.
		 */ 
		function reset() : void;
        
        /**
		 * A function to serialize an arbitrary object, even if it is not an instance
		 * of Serializable. In that case the implementation of this function should
		 * decide what to do.
		 */ 
        function serialize(obj:*) : void ;
        
    }
}
