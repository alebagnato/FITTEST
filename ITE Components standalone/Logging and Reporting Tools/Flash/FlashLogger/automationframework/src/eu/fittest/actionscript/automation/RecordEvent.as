package eu.fittest.actionscript.automation
{
	import flash.events.Event;
	
	/**
	 * A RecordEvent is dispatched by the Automation instance whenever a (UI) Event is detected.
	 */ 
	public class RecordEvent extends Event
	{
		/**
		 * @eventType "eu.fittest.Automation.RECORD"
		 */ 
		public static const RECORD : String = "eu.fittest.Automation.RECORD";
		
		private var _source:Delegate;
		private var _cmd:Command;
		
		/**
		 *
		 * @param source The Delegate associated with the source of the captured Event 
		 * @param cmd     Command object representing the captured Event
		 */  
		public function RecordEvent(source:Delegate, cmd:Command)
		{
			super(RECORD);
			this._source = source;
			this._cmd = cmd;
		}
		/**
		 * The source of the recorded event
		 */ 
		public function get source():Delegate
		{
			return _source;
		}
		/**
		 * The command of the recorded event, representing the information of the captured (UI) Event
		 */  
		public function get cmd():Command
		{
			return _cmd;
		}
		
	}
}