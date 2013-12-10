package eu.fittest.fbk.efsm2ct.plugin.utils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;

public class GuiHelper {

	public static void showError(Shell shell, String msg, Exception e) {

		StringBuilder sb = new StringBuilder();
		sb.append(e.getMessage()).append('\n');

		if (e.getCause() != null) {
			sb.append(e.getCause().getMessage());
		}

		MessageDialog.openError(shell, msg, sb.toString());

	}
	
	public static MessageConsole findConsole(String name) {
	      ConsolePlugin plugin = ConsolePlugin.getDefault();
	      IConsoleManager conMan = plugin.getConsoleManager();
	      IConsole[] existing = conMan.getConsoles();
	      for (int i = 0; i < existing.length; i++)
	         if (name.equals(existing[i].getName()))
	            return (MessageConsole) existing[i];
	      //no console found, so create a new one
	      MessageConsole myConsole = new MessageConsole(name, null);
	      conMan.addConsoles(new IConsole[]{myConsole});
	      return myConsole;
	   }  

}
