package eu.fittest.eclipse.focus.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;

import eu.fittest.eclipse.focus.FocusLoader;

public class OpenHandler extends AbstractHandler implements IHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		FocusLoader.loadFocus(null);
		return null;
	}

}
