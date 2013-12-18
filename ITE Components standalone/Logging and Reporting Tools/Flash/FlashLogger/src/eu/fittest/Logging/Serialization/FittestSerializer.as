/*

Authors: Wishnu Prasetya, Alexander Elyasov, Arie Middelkoop

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/
package eu.fittest.Logging.Serialization
{
	
	import flash.utils.*;
	
	/**
	 * A concrete implementation of SerializerBase. It writes
	 * to a bytearray for more efficiency. It produces a log
	 * in the format of raw FITTEST log file.
	 */ 
	public class FittestSerializer extends SerializerBase
	{

		/**
		 * The result of the serialization will be put here.
		 */ 
		public var result : ByteArray ;
		
		/**
		 * A help variable to keep track whether we have an open paragraph
		 * or not.
		 */  
		private var inParagraph : Boolean ;
		
		/**
		 * A stack to keep track what is the type of the object which is
		 * currently being processed.
		 */ 
		private var objectTypesStack : Array ;
		
		public function FittestSerializer()
		{
			result = new ByteArray() ;
			objectTypesStack = new Array() ;
			super() ;
		}
		
        /**
         * Additional reset specific for this serializer, which is used
         * to clear the result-byteArray and in-paragraph tracker.
         */
		public function resetByteArrayAndParTracker() : void {
			// result.clear() ; DOES NOT work with flex compiler 3.6, turning it off..
			result = new ByteArray() ;
			inParagraph = false ;
		}
		
		private function write(s : String) : void { 
			result.writeUTFBytes(s) ;
		}
		
		private function writeLine(s : String) : void { 
			result.writeUTFBytes(s) ;
			result.writeUTFBytes("\n") ;
		}
		
		private function openParagraph() : void {
			if (inParagraph) return ; // already open...
			inParagraph = true ;
			writeLine("%<P") ;
		}
		
		private function closeParagraph() : void {
			if (!inParagraph) return ; // no open paragraph to close.. so we return.
			inParagraph = false ;
			writeLine("%>") ;
		}
		
		private function writeFieldVal(name: Object, val:Object, ty:Object) : void {
			// re-open paragraph if it was close:
			openParagraph() ;
			write("%<{ ") ;
			write(name.toString()) ;
			write("=") ;
			write(val.toString()) ;
			write(":") ;
			write(ty.toString()) ;
			writeLine(" }%>") ;
		}
		
		private function writeVal(val:Object, ty:Object) : void {
			write("%<P %<{ ") ;
			write(val.toString()) ;
			write(":") ;
			write(ty.toString()) ;
			writeLine(" }%> %>") ;
		}
		
		override public function beginObject(obj : Object, objType : Object) : void {
			objectTypesStack.push(objType) ;
			write("%<S \"O:") ;
			write(objType.toString()) ;
			writeLine("\"");
			openParagraph() ;
			super.beginObject(obj,objType) ;
		}
		
		override public function endObject() : void { 
			var objType:Object = objectTypesStack.pop() ;
			closeParagraph()  ;
			writeLine("%>") ;
			super.endObject() ;
		}
		
		override public function serializeUID(ID : uint) : void {
			write("%<{ I=") ;
			write(ID.toString()) ;
			writeLine(":ID }%>") ;
		}
		
		override public function serializeUndefinedField(name : Object) : void {
			writeFieldVal(name,"undefined","void") ;
		}
		
		override public function serializeUndefined() : void {
			writeVal("undefined","void") ;
		}
		
		override public function serializeNullField(name : Object) : void {
			writeFieldVal(name,"null","Null") ;
		}
		
		override public function serializeNull() : void {
			writeVal("null","Null") ;
		}
		
		override public function serializeIntField(name : Object, val : int) : void {
			writeFieldVal(name,val,"int") ;
		}
		
		override public function serializeInt(val : int) : void {
			writeVal(val,"int") ;
		}
		
		override public function serializeNumberField(name : Object, val : Number) : void {
			writeFieldVal(name,val,"Number") ;
		}
		
		override public function serializeNumber(val : Number) : void {
			writeVal(val,"Number") ;
		}
		
		override public function serializeBooleanField(name : Object, val : Boolean) : void {
			writeFieldVal(name,val,"Boolean") ;
		}
		
		override public function serializeBoolean(val : Boolean) : void {
			writeVal(val,"Boolean") ;
		}
		
		override public function serializeStringField(name : Object, val : String) : void {
			// re-open paragraph if it was close:
			openParagraph() ;
			write("%<{ ") ;
			write(name.toString()) ;
			write("=\"") ;
			write(val.toString()) ;
			writeLine("\":String }%>") ;
		}
		
		override public function serializeString(val : String) : void {
			write("%<P %<{ \"") ;
			write(val.toString()) ;
			writeLine("\":String }%> %>") ;;
		}

		override public function serializeArrayField(val : *) : void {
		  storeField(Object("elem"), val);
		}

		override public function beginArray(): void {
		  write("%<S \"O:Array\"") ;
		  openParagraph() ;
		}
		
		override public function endArray(): void {
		  closeParagraph()  ;
		  writeLine("%>") ;
		}


		override public function serializePreviousField(name : Object, ID : uint) : void {
            openParagraph() ;
			write("%<{ ") ;
			write(name.toString()) ;
			write("=^") ;
			write(ID.toString()) ;
			writeLine(" }%>") ;
		}
		
		override public function serializeFieldName(name : Object) : void {
            openParagraph() ;
			write("%<{ ") ;
			write(name.toString()) ;
			write("=>") ;
			writeLine(" }%>") ;
			closeParagraph() ;
		}
		
		override public function serializeUnserializableField(name : Object, val:Object, ID : uint): void {
			writeFieldVal(name,"??",getQualifiedClassName(val)) ;
		}
		
		override public function serializeUnserializable(val:Object, ID : uint) : void {
			writeVal("??",getQualifiedClassName(val)) ;
		}
	}
}