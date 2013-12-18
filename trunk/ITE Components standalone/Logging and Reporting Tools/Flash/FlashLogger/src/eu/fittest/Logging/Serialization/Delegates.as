/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging.Serialization {
	
    import flash.utils.* ;
	
	/**
	 * The logger can serialize the state of an object of class C, but it needs to know 
	 * how it should do this, e.g. which fields are to be included. One way
	 * to specify this is by making C implements the interface Serializable. This however 
	 * requires the code of C to be extended; thus to some extent clutters it.
	 *
	 * <p>Another way to specify the serialization of instances of C is by writing a 
	 * delegate function that will do the job. This class maintains a static dictionary
	 * that contains entries of the form (C,f), where C is a class and and f is the 
	 * corresponding function acting as a delegate for serialization. When a Serializer
	 * (a subclass of SerlializerBase, to be more precise) in called to serialize an object,
	 * it always consults the dictionary first, to see if there is a delegate associated to
	 * the object's class. If there is, the delegate will be used.
	 * New delegates can indeed be added into the dictionary.
	 * </p>
	 * 
	 */ 
    public class Delegates {
		
		/**
		 * A Dictionary to maintain the mapping between classes and their
		 * serialization delegates. 
		 * 
		 * Note: Dictionary uses pointer comparison to match keys.
		 */ 
        private static var _delegates : Dictionary = new Dictionary();

		/**
		 * Register a function to act as the serialization-delegate of the
		 * given class. 
		 * 
		 * Such a function should accomodate two parameters; the first is 
		 * the object to serialize, the second parameter is the Serializer to use.
		 */ 
        public static function registerDelegate(cl : Class, del : Function) : void {
            _delegates[cl] = del;
        }

		/**
		 * Get a serialization-degelate for an object. The object should not
		 * be null or undefined. If the object's class is registred, this
		 * will return the corresponding delegate-function. Else the function
		 * will look up if there is a superclass registered, and return this
		 * superclass' delegate.
		 * 
		 * <p>If it cannot find a match, it will return a null.</p>
		 * 
		 */ 
        public static function getDelegate(obj : Object) : Function {
            if (obj == null)
                return null;  // you cannot associate a delegate with the 'null' value
                
            // WP: this does not work if we only have a delegate registered for the
            // object superclass.. so I change this a bit       
            //            
            // return _delegates[obj.constructor];
			
			var constr:* ;
			var clz : Class ;
			try { 
				// try to inspect the object's class:
				constr = obj.constructor ;
				clz = constr as Class ;
			}
			catch (e:Error) {
				// uhoh... the object has no pointer to its own class object,
				// then try to get it like this (less efficient):
				try {
					var cname : String = getQualifiedClassName(obj) ;
					clz = getDefinitionByName(cname) as Class ;
				}
				catch (e : ReferenceError) { return null ; }
			}
			
			if (clz==null) return null ;
				
            if (_delegates[clz] != undefined
				&&
				_delegates[clz] != null
				) 
				return _delegates[clz] ;
            
            // Arie has some idea to handle this; for now I will just solve it like this. 
            for (var C:Object in _delegates) {
               if (obj is (C as Class)) {
                   var fd : Function = _delegates[C] ;
                   _delegates[clz] = fd ;
                   return fd ;
               }
            }
            
            return null ;
        }
    }
}
