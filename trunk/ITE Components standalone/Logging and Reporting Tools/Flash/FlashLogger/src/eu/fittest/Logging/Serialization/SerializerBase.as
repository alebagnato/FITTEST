/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging.Serialization {

	import flash.utils.Dictionary;

	/**
	 * This class provides a skeleton implementation of the Serializer interface.
	 * The main task of this skeleton is to
	 * do the administration. It does not do any actual serialization work. To provide
	 * an actual serializer we extends this class, and implements the actual
	 * serializer there.
	 * 
	 * <p>This skeleton provides automatic traversal over an object graph. When an 
	 * an object is given to be serialized, the serializer may have to traverse its entire 
	 * structure can; this depends on the how the serialization of the object was
	 * specified. In any case, if the serializer has to traverse the structure, we should
	 * keep in mind that the structure can contain cycles. The serializer needs to keep
	 * track of sub-objects it has visited in order to avoid following non-termination
	 * when it follows a cycle. This class provides the administration of this.</p>
	 * 
	 * <p>This class does not enforce any maximum in the depth of the object graph it
	 * traverses. However, a concrete serializer can of course override this.</p>
	 * 
	 */
	public class SerializerBase implements Serializer {
		
		/**
		 * A dictionary to keep track of objects which have been serialized
		 * so far. If during traversal we encounter an object that we have
		 * seen before, we will not traverse that object again.
		 * For each object, we also assign a unique ID. A concrete serializer
		 * can show/log this ID if it wants.
		 */ 
		public var visited : Dictionary;	
		
		/**
		 * A free ID for objects.
		 */ 
		public var freshID : uint ;
		
		/**
		 * Current depth in the object structure, with respect to its root.
		 */ 
     	public var currentDepth : uint ;
		
		public function SerializerBase() : void {
			reset();
		}

		/**
		 *  Reset the state of the serializer, namely its table of
         *  visited objects.
		 */ 
		public function reset() : void {
			visited  = new Dictionary(true);
			freshID = 0 ;
			currentDepth = 0 ;
		}

		public function beginObject(obj : Object, objType : Object) : void {
			var ID : uint = addObject(obj) ;
			serializeUID(ID) ;
			currentDepth++ ;
			
		}
		
		private function addObject(obj : Object) : uint {
			var id : uint = freshID++ ;
			visited[obj] = id ;
			return id ;
			
		}
		
		public function endObject() : void { 
			currentDepth -- ;
		}
		
		public function storeField(name:Object, val:*) : void {
			storeFieldWorker(name,val) ;
		}
		
		private function storeFieldWorker(name:Object, val:*) : void {
			// handling undefined and null :
			if (val === undefined) {
				if (name==null) serializeUndefined() ;
				else serializeUndefinedField(name) ;
				return ;
			}
			if (val === null) {
				if (name==null) serializeNull() ;
				else serializeNullField(name) ;
				return ;
			}
			// handling primitive value:
			if (val is int) {
				if (name==null) serializeInt(val as int) ;
				else serializeIntField(name,val as int) ;
				return ;
			}
			if (val is Number) {
				if (name==null) serializeNumber(val as Number) ;
				else serializeNumberField(name, val as Number) ;
				return ;
			}
			if (val is Boolean) {
				if (name==null) serializeBoolean(val as Boolean) ;
				else serializeBooleanField(name, val as Boolean) ;
				return ;
			}
			if (val is String) {
				if (name==null) serializeString(val as String) ;
				else serializeStringField(name, val as String) ;
				return  ;
			}
            /* WP NOTE: turning off this branch, because its semantics is 
               not as intended. To discuss with Alexander.
			if (val is Array) {
				if (name!=null) serializeFieldName(name);
				beginArray();
				for each (var item in val) {
				  trace("loop");
				  serializeArrayField(item)
				}
				endArray();
				return ;
			}
            */
			// the object has been visited before:
			var ID:* = visited[val] ;
			if (ID != undefined) {
				// no recursion
				serializePreviousField(name,ID as uint) ;
				return ;
			}
			// the object has not been visited before, and it is serializable:
			if (val is Serializable) {
				// recursive case...
				if (name!=null) serializeFieldName(name) ;
				val.serializeSelf(this) ;
				return ;
			}
			var serializationDelegate : Function = Delegates.getDelegate(val) ;
			if (serializationDelegate != null) {
				// recursive case...
				if (name!=null) serializeFieldName(name) ;
				var df : Function = serializationDelegate as Function ;
				df(val,this) ;
				return ;
			}
			// note: array, collection, and dictionary will be handled
			// through delegates.
			
			// if none of the above cases match, the object is not serializable.
			var ID_ : uint = addObject(val) ;
			if (name==null) serializeUnserializable(val,ID_) ;
			else serializeUnserializableField(name,val,ID_) ;
		}
		
		/**
		 * Override this.
		 */
		public function serializeUID(ID : uint) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing an undefined value. Override this.
		 */
		public function serializeUndefinedField(name : Object) : void {
			// override this
		}
		
		/**
		 * To an serialize undefined value. Override this.
		 */
		public function serializeUndefined() : void {
			// override this
		}
		
		/**
		 * To serialize a field containing a null. Override this.
		 */		
		public function serializeNullField(name : Object) : void {
			// override this
		}
		
		/**
		 * To serialize null. Override this.
		 */		
		public function serializeNull() : void {
			// override this
		}
		
		/**
		 * To serialize a field containing an integer. Override this.
		 */		
		public function serializeIntField(name : Object, val : int) : void {
			// override this
		}
		
		/**
		 * To serialize an integer. Override this.
		 */		
		public function serializeInt(val : int) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing an number. Override this.
		 */		
		public function serializeNumberField(name : Object, val : Number) : void {
			// override this
		}
		
		/**
		 * To serialize a number. Override this.
		 */		
		public function serializeNumber(val : Number) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing a boolean value. Override this.
		 */		
		public function serializeBooleanField(name : Object, val : Boolean) : void {
			// override this
		}
		
		/**
		 * To serialize a boolean value. Override this.
		 */		
		public function serializeBoolean(val : Boolean) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing a string. Override this.
		 */		
		public function serializeStringField(name : Object, val : String) : void {
			// override this
		}
		
		/**
		 * To serialize a string. Override this.
		 */	
		public function serializeString(val : String) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing an array. Override this.
		 */
		public function serializeArrayField(val : *) : void {
			// override this
		}
		
		/**
		 * To serialize an array; indicating its start. Override this.
		 */		
		public function beginArray() : void {
		      // override this
		}
		
		/**
		 * To serialize an array; indicating its end. Override this.
		 */
		public function endArray() : void {
		      // override this
		}
		
		/**
		 * Override this.
		 */
		public function serializePreviousField(name : Object, ID : uint) : void {
			// override this
		}
		
		/**
		 * To serialize a field name. Override this.
		 */		
		public function serializeFieldName(name : Object) : void {
			// override this
		}
		
		/**
		 * To serialize a field containing a value that cannot be serialized. Override this.
		 */
		public function serializeUnserializableField(name : Object, val:Object, ID : uint) : void {
			// override this
		}
		
		/**
		 * To serialize a value that cannot be serialized (by indicating that it is unserializable). Override this.
		 */		
		public function serializeUnserializable(val:Object, ID : uint) : void {
			// override this
		}
		
		/**
		 * To serialize an object or value.
		 */	
		public function serialize(val:*) : void {
			storeFieldWorker(null,val) ;
		}

    }
}
