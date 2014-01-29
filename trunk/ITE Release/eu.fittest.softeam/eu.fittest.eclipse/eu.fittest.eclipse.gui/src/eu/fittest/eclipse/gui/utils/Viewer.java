package eu.fittest.eclipse.gui.utils;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;

import eu.fittest.eclipse.gui.Activator;

public class Viewer {
	static public final Image CHECKED_ICON = Activator.getImageDescriptor("resources/icons/checked16x16.png").createImage();
	static public final Image UNCHECKED_ICON = Activator.getImageDescriptor("resources/icons/unchecked16x16.png").createImage();
	
	static public TableViewerColumn createTableViewerColumn(TableViewer viewer, String title, int bound, final int colNumber) {
		final TableViewerColumn viewerColumn = new TableViewerColumn(viewer, SWT.NONE);
		final TableColumn column = viewerColumn.getColumn();
		column.setText(title);
		column.setWidth(bound);
		column.setResizable(true);
		column.setMoveable(true);
		return viewerColumn;		
	}
	
	static public IViewPart getView(String id){
		IViewPart result = null;
		IWorkbench wb = PlatformUI.getWorkbench();
	    IWorkbenchWindow win = wb.getActiveWorkbenchWindow();
	    if(win!=null){
			IWorkbenchPage page = win.getActivePage();
			if(page!=null){
				result = page.findView(id);
			}
	    }
	    return result;
	}
	
	static public MessageConsole findConsole(String name) {
	      ConsolePlugin plugin = ConsolePlugin.getDefault();
	      IConsoleManager conMan = plugin.getConsoleManager();
	      IConsole[] existing = conMan.getConsoles();
	      MessageConsole console = null;
	      int i = 0;
	      while(console==null && i<existing.length){
	         if (name.equals(existing[i].getName())){
	            console = (MessageConsole) existing[i];
	         }
	         i++;
	      }
	      
	      if(console==null){
		      console = new MessageConsole(name, null);
		      conMan.addConsoles(new IConsole[]{console});
	      }
	      return console;
	   }
}
