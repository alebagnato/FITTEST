/*

Authors: Wishnu Prasetya, Alexander Elyasov

Copyright 2012 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/
package eu.fittest.Logging {

   import eu.fittest.Logging.Serialization.*; 
   import flash.utils.Dictionary;
   
      
   /**
    * This class provides a representation of application state. It
    * allows object-fields to be added dynamically. They will be
    * included in the serialization.
    * Weak pointers are used; so when the object that owns fields f1..fk
    * are no longer used, the dictionary used to keep track of them
    * will not prevent them from being garbage collected.
    *
    * To register a field we do :
    *
    *    registerObject(o,"someUniqueId") ;
    *    registerField(o,fieldName1) ;
    *    registerField(o,fieldName2) ;
    *     
    */     
   public class DynAppState implements Serializable {
   
    
      /**
       * This is used to maintain the mapping (o,id,fns) where o
       * is an object that will be part of the the state representation,
       * and fns is a list of fields of o which are included in
       * the state.
       *
       * Id is an id associated to the object o; this id must be unique
       * for each object in myState. If a new object with the same id
       * is later registered, then existing objects with the same id will
       * be unlinked from myState.
       */
      private var myState : Dictionary ;
      
      // this is supposed to be a private constructor to facilitate the
      // singleton pattern we used here; but AS3 does not allow a constructor
      // to be private! 
      public function DynAppState() {
         if (theInstance != null) throw new Error("An instance of this class already exists. Only one instance is allowed!") ;
         myState = new Dictionary(true);  // using weak keys!
      }
      
      // singleton pattern:
      static private var theInstance : DynAppState = null ;
      
      // to get the single instance of this class:
      static public function getInstance() : DynAppState {
         if (theInstance == null) theInstance = new DynAppState() ;
         //trace("** DynAppState instance exists") ;
         return theInstance ;
      }
      
      // get the ID of an object in MyState. The object is assumed to be
      // registered.
      private function getID(o:Object) : String {
          return myState[o][0] ;
      }
      
      // get the fields of an object in MyState. The object is assumed to be
      // registered.
      private function getFields(o:Object) : Array {
          return myState[o][1] ;
      }
      
      private function objAlreadyRegistered(o:Object) : Boolean {
          if (myState[o] == undefined) return false ;
          if (myState[o] == null) return false ;
          return true ;
      }
      
      // check field, assuming o is already registered:
      private function fieldAlreadyRegistered(o:Object, fname:String) : Boolean {
          for each (var f : String in getFields(o)) {
             if (fname == f) return true ;
          }
          return false ;
      }
      
      private function unlinkID(id:String) : void {
         for (var o:Object in myState) {
           if (!objAlreadyRegistered(o)) continue ;
           if (getID(o) == id) // then clear it:
               myState[o] = null ;
         }
      }
      
      /**
       * Register an object to be part of myState. The id must be unique.
       * If there are other object(s) in myState that map to the same id,
       * they will be unlinked from myState.       
       */
      public function registerObject(o:Object, idFragment:String) : void {
         var objId : String = getObjId(o) ;
         if (objId == null) return ;  // obj has no id
         if (objAlreadyRegistered(o)) return ;
         if (objId.search(idFragment) < 0) return ;   // obj does not have a matching id
         unlinkID(objId) ;  // remove existing registration for the same id
         var odesc : Array = new Array() ;
         odesc[0] = objId ;
         odesc[1] = new Array() ;
         myState[o] = odesc ;              
      }
      
      
      private function getObjId(o:Object) : String {
          if (o["automationName"] == undefined) return null ;
          return o["automationName"] ;
      }
      

      /**
       * To register a field of an object into myState. The object must
       * have already been registered to myState, else this function will
       * not register the field.
       */
      public function registerField(o:Object, fname:String) : void {
         if (!objAlreadyRegistered(o)) return ;
         if (fieldAlreadyRegistered(o,fname)) return ;
         // field is not yet registered, so register it:   
         var fields : Array = getFields(o) ;
         var N : uint = fields.length ;
         fields[N] = fname ;
      }
      
      private var MYTYPE : String = "DynAppState" ;
      
      /*
      private function getter(o:Object, fn:String) : Function {
         var firstLetter : String = fn.charAt(0) ;
         var getterName : String = "get" + firstLetter.toUpperCase() + fn.substr(1) ;
         return o[getterName] ;
      }
      */
      
      public function serializeSelf(s : Serializer) : void {
        s.beginObject(this,Object(MYTYPE)) ;
        for (var o:Object in myState) {
           if (!objAlreadyRegistered(o)) continue ;
           var ID : String = getID(o) ;
           for each (var f:String in getFields(o)) {
              //var getter_ : Function = getter(o,f) ;
              //if (getter_ == null) continue ;
              //s.storeField(ID + "_" + f , getter_()) 
              if (o[f] == undefined) continue ;
              s.storeField(ID + "_" + f , o[f])
           }
        } ;
        s.endObject() ;
      }
     
   
   }
}