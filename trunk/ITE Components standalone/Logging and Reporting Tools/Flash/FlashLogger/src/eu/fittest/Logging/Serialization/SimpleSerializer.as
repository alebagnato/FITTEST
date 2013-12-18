package eu.fittest.Logging.Serialization
{
	/**
	 * A simplictic concrete implementation of SerializerBase.
	 * It serializes to a plain string.
	 */ 
	public class SimpleSerializer extends SerializerBase
	{
		public var result : String ;
		
		public function SimpleSerializer()
		{
			super() ;
		}
		
		override public function reset() : void {
			result = "" ;
			super.reset() ;
		}
		
		private function write(s : String) : void { 
			result = result + s + "\n" ;  
		}
		
		private function writeFieldVal(name: Object, val:Object) : void {
			write("   ** " + name + " = " + val) ;
		}
		
		private function writeVal(val:Object) : void {
			write("   " + val) ;
		}
		
		override public function beginObject(obj : Object, objType : Object) : void {
			write("<<  " + objType) ;
			super.beginObject(obj,objType) ;
		}
		
		override public function endObject() : void { 
			write(">>") ;
			super.endObject() ;
		}
		
		override public function serializeUID(ID : uint) : void {
			write("   ** XID = " + ID) ;
		}
		
		override public function serializeUndefinedField(name : Object) : void {
			write("   ** " + name + " = undef") ; 
		}
		
		override public function serializeUndefined() : void {
			write("   undef") ;
		}
		
		override public function serializeNullField(name : Object) : void {
			write("   ** " + name + " = null") ;
		}
		
		override public function serializeNull() : void {
			write("   null") ;
		}
		
		override public function serializeIntField(name : Object, val : int) : void {
			writeFieldVal(name,val) ;
		}
		
		override public function serializeInt(val : int) : void {
			writeVal(val) ;
		}
		
		override public function serializeNumberField(name : Object, val : Number) : void {
			writeFieldVal(name,val) ;
		}
		
		override public function serializeNumber(val : Number) : void {
			writeVal(val) ;
		}
		
		override public function serializeBooleanField(name : Object, val : Boolean) : void {
			writeFieldVal(name,val) ;
		}
		
		override public function serializeBoolean(val : Boolean) : void {
			writeVal(val) ;
		}
		
		override public function serializeStringField(name : Object, val : String) : void {
			writeFieldVal(name,val) ;
		}
		
		override public function serializeString(val : String) : void {
			writeVal(val) ;
		}
		
		override public function serializePreviousField(name : Object, ID : uint) : void {
			write("   ** " + name + " --> " + ID) ;
		}
		
		override public function serializeFieldName(name : Object) : void {
			write("   ** " + name) ;
		}
		
		override public function serializeUnserializableField(name : Object, val:Object, ID : uint): void {
			write("   ** " + name + " = ?? " + ID) ;
		}
		
		override public function serializeUnserializable(val:Object, ID : uint) : void {
			write("   ?? " + ID) ;
		}
	}
}