/*

Authors: Wishnu Prasetya, Alexander Elyasov, Arie Middelkoop

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging.Serialization
{

	
	import flash.utils.Dictionary;
	
	/**
	 * This class provides some serialization delegate-functions; e.g. for array, collection, and dictionary. 
	 */ 
	public class SomeSerializationDelegates
	{
		private static const ARRAY_TY_NAME : String = "Array" ;
		private static const COLL_TY_NAME : String = "Collection" ;
		private static const DICT_TY_NAME : String = "Dictionary" ;
		
		public static function arraySerializationDelegateFunction(a : Object, s : Serializer) : void {
			s.beginObject(a,ARRAY_TY_NAME) ;
			var A:Array = a as Array ;
			// if (A==null) throw new Error("DEBUG: A is null!") ;
			for (var i:uint = 0 ; i<A.length; i++) 
				s.storeField("elem",A[i]) ;
			s.endObject() ;
		}
		
		public static function collectionSerializationDelegateFunction(c : Object, s : Serializer) : void {
			s.beginObject(c,COLL_TY_NAME) ;
			for each (var o:* in c) {
				s.storeField("elem",o) ;
			}
			s.endObject() ;
		}
		
		public static function dictionarySerializationDelegateFunction(d : Object, s : Serializer) : void {
			s.beginObject(d,DICT_TY_NAME) ;
			var D:Dictionary = d as Dictionary ;
			for (var key:Object in D) {
				s.storeField("key",key) ;
				s.storeField("val",D[key]) ;
			}
			s.endObject() ;
		}
        
        /**
         * Register all the delegates provided in this class.
         */
        public static function registerUs() : void {
            Delegates.registerDelegate(Array,arraySerializationDelegateFunction) ;
            //Delegates.registerDelegate(Collection,collectionSerializationDelegateFunction) ;
            Delegates.registerDelegate(Dictionary,dictionarySerializationDelegateFunction) ;
        }
	}
}