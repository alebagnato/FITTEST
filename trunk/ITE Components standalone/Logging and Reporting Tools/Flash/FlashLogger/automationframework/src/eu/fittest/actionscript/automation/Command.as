package eu.fittest.actionscript.automation
{
	/**
	 * Captured/Replayable Event
	 */ 
	public class Command
	{
		private var _args:Array;
		private var _action:String;
		
		/**
		 * @param action The Command's action
		 * @param args The array of arguments of the Command
		 */ 
		public function Command(action:String, args:Array)
		{
			this._action = action;
			this._args = args;
		}
		
		/**
		 * The arguments of the Command
		 */ 
		public function get args():Array
		{
			return _args;
		}
		
		/**
		 * The action String of the Command
		 */ 
		public function get action():String
		{
			return _action;
		}
		
		/**
		 * Utility method to create a Command object
		 * 
		 * @param action The Commands action
		 * @param ...args The arguments of the Command
		 */ 
		public static function create(action:String, ...args):Command
		{
			return new Command(action,args);
		}
	}
}