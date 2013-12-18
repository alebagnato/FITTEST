/*

Authors: Wishnu Prasetya, Alexander Elyasov, Arie Middelkoop

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/
package eu.fittest.Logging.Serialization{

import mx.controls.* ;
import mx.core.* ;
import flash.utils.* ;

/**
 * Contians several serialization delegate-functions to serialize some mx GUI components.
 */ 
	public class MxUISerializableDelegates {
   
/**
 * To serialize a button.
 */ 		
   public static function buttonSerializationDelegate(b : Button, s : Serializer) : void {
      s.beginObject(b, Object(getQualifiedClassName(b))) ;
      s.storeField(Object("id"), b.id) ;
      s.storeField(Object("label"), b.label) ;
      s.endObject() ;
   }
   
   /**
   * To serialize a combo-box.
   */ 
   public static function comboboxSerializationDelegate(b : ComboBox, s : Serializer) : void {
      s.beginObject(b, Object(getQualifiedClassName(b))) ;
      s.storeField(Object("id"), b.id) ;
      s.storeField(Object("selectedItem.label"), b.selectedItem.label) ;
      s.endObject() ;
   }
   
   /**
   * To serialize a text element.
   */ 
   public static function textSerializationDelegate(t : Text, s : Serializer) : void {
      s.beginObject(t, Object(getQualifiedClassName(t))) ;
      s.storeField(Object("id"), t.id) ;
      s.storeField(Object("text"), t.text) ;
      s.endObject() ;
   }
   
   /**
   * To serialize a text-input element.
   */ 
   public static function textInputSerializationDelegate(t : TextInput, s : Serializer) : void {
      s.beginObject(t,getQualifiedClassName(t)) ;
      s.storeField(new QName("id"), t.id) ;
      s.storeField(new QName("text"), t.text) ;
      s.endObject() ;
   }
      
}
}