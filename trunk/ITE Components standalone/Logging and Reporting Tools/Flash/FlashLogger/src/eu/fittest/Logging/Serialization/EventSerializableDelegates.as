/*

Authors: Wishnu Prasetya, Alexander Elyasov, Arie Middelkoop

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/
package eu.fittest.Logging.Serialization {

	import eu.fittest.actionscript.automation.*;
	
	import flash.events.*;
	import flash.utils.*;

	/**
	 * Contains serialization delegate-functions to serialize Flash events.
	 */ 
	public class EventSerializableDelegates {
   
		/**
		 * To serialize a flash-event.
		 */ 
		public static function serializationDelegate(e : Event, s : Serializer) : void {
			s.beginObject(e,Object(getQualifiedClassName(e))) ;
			s.storeField(Object("type"), e.type) ;
			s.storeField(Object("target"), e.target) ;
			s.endObject() ;
		}
		
		/**
		 * A specific serializer to serialize a record-event produced by the Automation-framework.
		 * Such a record-event represents an intercepted event, which is a normal event intercepted by 
		 * the Automation-framework, for the purpose of logging it. The record-event will contain a
		 * pointer to the intercepted event. Furthermore, the log syntax requires it to be formatted
		 * in a specific way.
		 */ 		
		public static function recordEventserializationDelegate(e : RecordEvent, s : Serializer) : void {
			//trace("--"+getQualifiedClassName(e.source));
			//trace("--"+e.source.id);
			s.beginObject(e,Object(getQualifiedClassName(e))) ;
			s.storeField(Object("targetID"), e.source.id) ;
			s.storeField(Object("type"), e.cmd.action) ;
			s.storeField(Object("args"), e.cmd.args) ;
			s.endObject() ;
		}

}
}