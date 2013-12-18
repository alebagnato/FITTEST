package {

  import eu.fittest.Logging.*;
  import eu.fittest.Logging.Serialization.*;
  import eu.fittest.actionscript.automation.*;
  import flash.utils.*;
  
  import mx.controls.*;
  import mx.core.*;
  import mx.containers.*;
	
  /**
   * This class is the concrete implementation of FittestLoggerHook. 
   */
  public class MyLogger extends FittestLoggerHook {
  
    public function MyLogger() {		
	  super() ;
    }
	
    // Specify the ignore-list, if applicable; events mathing this function will be 
	// igonred/not logged:
    override public function ignoreList(evt : RecordEvent) : Boolean {
        var o = evt.source.target ;
        if(o is mx.controls.Label
            && evt.source.id.substr(0,5) == "Label") // probably non-semantical label, ignore
            return true ;
        if(o is mx.containers.Canvas 
             && (evt.source.id.substr(0,3) != "IDY"))  // probably non-semantical canvas, ignore
            return true ;         
        return false ;
    }

    // specify the serialization delegates to use:
    override public function registerSerializationDelegates() : void {
        super.registerSerializationDelegates() ;
        // For example, we usually want to register a serializer for the application
		// itself, so that we can log its state:
        Delegates.registerDelegate(getDefinitionByName("flexstore") as Class,serializationFunctionOfMyApp) ;  
    }
	
	// This specifies how the state of the application will be serialized into the log.
	// Serializing its whole concrete state is usually too much; so, specify here
	// which information is to be logged:
    public static function serializationFunctionOfMyApp(app : Object, s : Serializer) : void {
        var numOfSelectedItems : int = app["pView"]["filterPanel"]["filter"]["count"] ;
        var numInShopCart : int      =  app["pView"]["cartPanel"]["numProducts"] ;
        var cartTotal : String       =  app["pView"]["cartPanel"]["grandTotal"]["text"] ;
        var numInCompareCart: int    = app["pView"]["filterPanel"]["productList"]["items"]["length"];
        var catalogContents: Array   = app["pView"]["catalogPanel"]["getCatalog"];
        var shoppingCartContents: Array = app["pView"]["cartPanel"]["productList"]["getCart"];

        // the serialization begin here:
        s.beginObject(app,"AppAbstractState") ;
        s.storeField(new QName("numOfSelectedItems"), numOfSelectedItems) ;
        s.storeField(new QName("numInShopCart"), numInShopCart) ;
        s.storeField(new QName("cartTotal"), cartTotal) ;
        s.storeField(new QName("numInCompareCart"), numInCompareCart) ;
        s.storeField(new QName("catalogContents"), catalogContents);
        s.storeField(new QName("shoppingCartContents"), shoppingCartContents);
        s.endObject() ;
    }
   
  }	
}