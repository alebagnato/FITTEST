package es.cv.gvcase.fefem.common.composites;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.internal.Messages;
import es.cv.gvcase.fefem.common.widgets.ChooseDialog;

public abstract class EMFPropertyMultipleEReferenceComposite extends
		EMFContainedCollectionEditionComposite {

	private List<EObject> choices = new ArrayList<EObject>();
	
	public EMFPropertyMultipleEReferenceComposite(Composite parent, int style,
			FormToolkit toolkit, EObject eobject, FEFEMPage page) {
		super(parent, style, toolkit, eobject, page);
		updateChoices();
	}

	public EMFPropertyMultipleEReferenceComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
		updateChoices();
	}

	@Override
	protected void createAddAndRemoveButtons(Composite container,
			FormToolkit toolkit) {

			createAddButton(container,getButtonLabel(ADDELEMENT_BUTTON_ID), toolkit);
			
			createRemoveButton(container,getButtonLabel(REMOVEELEMENT_BUTTON_ID), toolkit);
	}

	
	@Override
	protected String getButtonLabel(int buttonID) {
		switch (buttonID){
			case ADDELEMENT_BUTTON_ID: return Messages.EMFPropertyMultipleEReferenceComposite_AddReference;
			case REMOVEELEMENT_BUTTON_ID: return Messages.EMFPropertyMultipleEReferenceComposite_RemoveReference;
			default: return super.getButtonLabel(buttonID);
		}
	}

	@Override
	protected SelectionListener getAddButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				updateChoices();
				ChooseDialog dialog = new ChooseDialog(getShell(), getChoices().toArray());
				dialog.setLabelProvider(getChooserLabelProvider());

				if (dialog.open() == Window.OK) {
					Object[] selection = dialog.getResult();

					if (selection != null && selection.length > 0 && modelObservable != null) {

							handleSelection(selection);
					}
				}	
			}		
		};
		
		return adapter;
	}
	
	protected SelectionListener getRemoveButtonSelectionListener() {
		SelectionAdapter adapter = new SelectionAdapter()  {
			public void widgetSelected(SelectionEvent event) {
				if(getViewer().getSelection() instanceof StructuredSelection) {
					List elementsToDelete = ((StructuredSelection)getViewer().getSelection()).toList();
					if(elementsToDelete.size() > 0) {
						modelObservable.removeAll(elementsToDelete);
						getPage().setDirty(true);
						if(getViewer().getTable().getItems().length > 0){
							getViewer().setSelection(new StructuredSelection(
									getViewer().getElementAt(0)));
						}
					}
				}
			}

		};
		return adapter;
	}
	
	protected void handleSelection(Object[] selection) {
		modelObservable.addAll(Arrays.asList(selection));
		getPage().setDirty(true);
		getViewer().setSelection(new StructuredSelection(selection));
	}	
	
	public void setChoices(List<EObject> choices){
		this.choices = choices;
	}
	
	public List<EObject> getChoices(){
		List<EObject> tmpList = new ArrayList<EObject>(this.choices);
		tmpList.removeAll(this.modelObservable);
		return tmpList;
	}
	
	protected abstract void updateChoices();
	protected abstract ILabelProvider getChooserLabelProvider();

}