package
{
    import flash.display.Sprite;
    import flash.utils.Dictionary;

    // import objects
    import flash.utils.ByteArray;
    import flash.net.SharedObject;


    public final class test2 extends Sprite
    {
        public static function createTable() : flash.utils.Dictionary {
          var dict : flash.utils.Dictionary = new flash.utils.Dictionary();

          dict[flash.utils.Dictionary] = [
            flash.utils.Dictionary
            , [ flash.net.SharedObject ]
            , [ Object ]
            ];

          return dict;
        }

        /*
        private const pred_flash_utils_Dictionary : Array = [ ];
        private const succ_flash_utils_Dictionary : Array = [ ];
        private const info_flash_utils_Dictionary : Array =
          [ pred_flash_utils_Dictionary, succ_flash_utils_Dictionary ];

        public const InhInfo : Object = { Dictionary: succ_flash_utils_Dictionary };
        */

        public function test2() : void {
          trace("started.");
        }
    }
}
