package es.cv.gvcase.fefem.common;


import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.edit.ui.action.EditingDomainActionBarContributor;
import org.eclipse.emf.edit.ui.action.LoadResourceAction;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;

import es.cv.gvcase.fefem.action.loadresource.FilteredLoadResourceAction;
import eu.fittest.eclipse.testproject.formeditor.Activator;

public abstract class FEFEMEditorContributor extends EditingDomainActionBarContributor {

	private IEditorPart activeEditorPart;
	
	/**
	  * This is the action used to load a resource.
	  */
	protected LoadResourceAction loadResourceAction;

		
	/**
	 * Creates a multi-page contributor.
	 */
	public FEFEMEditorContributor() {
		super();
		createActions();
	}
	
	
	/**
	 * Returns the action registed with the given text editor.
	 * @return IAction or null if editor is null.
	 */
	protected IAction getAction(ITextEditor editor, String actionID) {
		return (editor == null ? null : editor.getAction(actionID));
	}
	/* (non-JavaDoc)
	 * Method declared in AbstractMultiPageEditorActionBarContributor.
	 */

	public void setActivePage(IEditorPart part) {
		if ((activeEditorPart == part)&&(part == null))
			return;

		activeEditorPart = part;

		IActionBars actionBars = getActionBars();
		if (actionBars != null) {

			ITextEditor editor = (part instanceof ITextEditor) ? (ITextEditor) part : null;

			actionBars.setGlobalActionHandler(
				ActionFactory.DELETE.getId(),
				getAction(editor, ITextEditorActionConstants.DELETE));
			actionBars.setGlobalActionHandler(
				ActionFactory.UNDO.getId(),
				getAction(editor, ITextEditorActionConstants.UNDO));
			actionBars.setGlobalActionHandler(
				ActionFactory.REDO.getId(),
				getAction(editor, ITextEditorActionConstants.REDO));
			actionBars.setGlobalActionHandler(
				ActionFactory.CUT.getId(),
				getAction(editor, ITextEditorActionConstants.CUT));
			actionBars.setGlobalActionHandler(
				ActionFactory.COPY.getId(),
				getAction(editor, ITextEditorActionConstants.COPY));
			actionBars.setGlobalActionHandler(
				ActionFactory.PASTE.getId(),
				getAction(editor, ITextEditorActionConstants.PASTE));
			actionBars.setGlobalActionHandler(
				ActionFactory.SELECT_ALL.getId(),
				getAction(editor, ITextEditorActionConstants.SELECT_ALL));
			actionBars.setGlobalActionHandler(
				ActionFactory.FIND.getId(),
				getAction(editor, ITextEditorActionConstants.FIND));
			actionBars.setGlobalActionHandler(
				IDEActionFactory.BOOKMARK.getId(),
				getAction(editor, IDEActionFactory.BOOKMARK.getId()));
			actionBars.updateActionBars();
			
			
		}
		
		
	}
	@Override
	public void setActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);
		
		if(part!=null){
			loadResourceAction.setActiveWorkbenchPart(part);
		}
	}
	protected void createActions() {
		if(isFilteredLoadResourceAction())
			loadResourceAction = new FilteredLoadResourceAction();
		else{
			loadResourceAction = new LoadResourceAction();
			loadResourceAction.setImageDescriptor(new ImageDescriptor(){

				@Override
				public ImageData getImageData() {
					String imagePath = "";
					try {
						imagePath = FileLocator.toFileURL(
								Platform.getBundle(Activator.PLUGIN_ID)
										.getResource("icons/full/etool16")).getPath();
						imagePath += "loadresource.gif";
						Image image = new Image(Display.getCurrent(), imagePath);
						
						return image.getImageData();
					} catch (IOException e) {
					}
	
					return null;
					
				}
				
			});
		}
		
	}
	public void contributeToMenu(IMenuManager manager) {
		IMenuManager menu = new MenuManager("For&msEditor","es.cv.gvcase.fefem.editormenu");
		manager.prependToGroup(IWorkbenchActionConstants.MB_ADDITIONS, menu);
		menu.add(loadResourceAction);
		
	}
	public void contributeToToolBar(IToolBarManager manager) {
		manager.add(new Separator());
		manager.add(loadResourceAction);
		
	}
	public LoadResourceAction getLoadResourceAction() {
		return loadResourceAction;
	}

	/**
	 * May be overrided by subclasses if want to use FilteredLoadResourceAction 
	 * @return
	 */
	public boolean isFilteredLoadResourceAction() {
		return false;
	}
	
	
}
