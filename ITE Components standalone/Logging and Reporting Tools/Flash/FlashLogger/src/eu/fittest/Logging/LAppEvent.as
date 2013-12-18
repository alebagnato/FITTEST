/*

Authors: Alexander Elyasov, Wishnu Prasetya

Copyright 2012 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging
{

    import eu.fittest.Logging.Serialization.* ;
    import flash.utils.*;
    
    /**
	 * To abstractly represent an application event. This can be used if
     * the target application does not want to fix a specific representation
     * for application events.
	 * 
	 */ 
	public class LAppEvent implements Serializable {
    
        public var type : String ;
        public var targetID : String ;
        public var argz : Array ;
        
        public function LAppEvent(ty:String, tid:String, ...args) {
           type = ty ;
           targetID = tid ;
           this.argz = args ;
        }
        
        public function serializeSelf(s : Serializer) : void {
            s.beginObject(this,Object(getQualifiedClassName(this))) ;
            s.storeField("targetID",targetID) ;
			s.storeField("type",type) ;	
            s.storeField("args",argz) ;
			s.endObject() ;       
        }
        
        static private var LAPPEVENT : LAppEvent = null ;
        
        static public function getLAppEvent() : LAppEvent {
           if (LAPPEVENT == null) LAPPEVENT = new LAppEvent("","") ;
           return LAPPEVENT ;
        }
        
    }

}