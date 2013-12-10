package eu.fittest.common.services.command.spec;

import eu.fittest.common.core.service.ServiceEvent;




public class CommandEvent extends ServiceEvent<ILocalCommandService>{
    public CommandEvent(ILocalCommandService source) {
		super(source);
	}

	
    public String command;

    
    public CommandEventKind kind;


}
