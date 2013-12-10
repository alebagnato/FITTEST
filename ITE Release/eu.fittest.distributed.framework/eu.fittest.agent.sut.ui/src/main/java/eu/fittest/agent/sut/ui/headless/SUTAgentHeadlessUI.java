package eu.fittest.agent.sut.ui.headless;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Scanner;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.agent.sut.ui.AbstractAgentUI;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class SUTAgentHeadlessUI extends AbstractAgentUI{
	private ActionListener _registerAction = null;
	private ActionListener _exitAction = null;
	
	public SUTAgentHeadlessUI(){
		super();
	}
	
	public void open(){
		Scanner in = new Scanner(System.in);
	     while(_env==null){
			System.out.println("Select Environment:");
			System.out.println("\t1)Production");
			System.out.println("\t2)Test");
			System.out.println("\t?");
			int environment = 0;
			if(in.hasNextInt()){
				environment = in.nextInt();	
			}
			else{
				in.next();
			}
		     switch(environment){
		     case 1:
		    	 _env = HUTEnvironment.PRODUCTION;
		    	 break;
		     case 2:
		    	 _env = HUTEnvironment.TEST;
		    	 break;
		    default:
		    	System.out.println("Wrong input");
				break;
		     }
	     }
	     
	     while(_type==null){
	    	System.out.println("Select Host Type:");
			System.out.println("\t1)Server");
			System.out.println("\t2)Client");
			System.out.println("\t?");
			int	type = 0;
			if(in.hasNextInt()){
				type = in.nextInt();	
			}
			else{
				in.next();
			}
		     switch(type){
		     case 1:
		    	 _type = HUTType.SERVER;
		    	 break;
		     case 2:
		    	 _type = HUTType.CLIENT;
		    	 break;
		    default:
		    	System.out.println("Wrong input");
		    	break;
		     }
	     }
	     
	     boolean accepted = false;
	     while(!accepted){
	    	 System.out.println("Do you accept default description: "+_description+" (Y or N)?");
	    	 String value = in.next();
	    	 if("Y".equalsIgnoreCase(value)){
	    		 accepted =true;
	    	 }
	    	 else if("N".equalsIgnoreCase(value)){
	    		 System.out.println("Provide a description for this host:");
	    		 _description = in.nextLine();
	    		 _description = in.nextLine();
	    		 accepted = true;
	    	 }
	     }
	     
	     if(_registerAction!=null) _registerAction.actionPerformed(new ActionEvent(this, 0, "register"));
	     
	     boolean exit = false;
	     while(!exit){
	    	 System.out.println("Press E then Hit Enter to exit:");	   
		     String value = in.next();
		     exit = "E".equalsIgnoreCase(value);
	     }
	     if(_exitAction!=null) _exitAction.actionPerformed(new ActionEvent(this, 0, "exit"));
	}

	@Override
	public void close() {
		
	}

	@Override
	public void addExitActionListener(ActionListener l) {
		_exitAction = l;
	}
	
	@Override
	public void addRegisterActionListener(ActionListener l) {
		_registerAction = l;
	}

	@Override
	public synchronized void incomingEvent(ServiceEvent event) {
		System.out.println(event.getSource());		
	}

	@Override
	public void displayMessage(Level level, String message) {
		ITELogger.log(level, message);
	}

	@Override
	public void exit() {
		// TODO Auto-generated method stub
		
	}
}
