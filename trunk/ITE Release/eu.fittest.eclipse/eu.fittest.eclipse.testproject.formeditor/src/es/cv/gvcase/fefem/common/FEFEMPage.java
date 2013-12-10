package es.cv.gvcase.fefem.common;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

public abstract class FEFEMPage extends FormPage {

	protected boolean isDirty = false;
	
	protected FormToolkit toolkit = null;
	private FEFEMEditor editor = null;
	
	public FEFEMPage(FormEditor editor, String id, String title) {
		super(editor, id, title);
		if (editor instanceof FEFEMEditor){
			this.editor = (FEFEMEditor) editor;
		}
	}

	public Section createSection(final Composite parent, FormToolkit toolkit,
			String text, String description) {
		Section section = toolkit.createSection(parent, Section.EXPANDED
				| Section.TITLE_BAR | Section.DESCRIPTION);

		String textString = text != null ? text : "";
		String descriptionString = description != null ? description : "";

		section.setText(textString);
		section.setDescription(descriptionString);

		section.setLayoutData(new GridData(GridData.FILL_BOTH));

		return section;
	}
	
	public FEFEMEditor getEditor(){
		return this.editor;
	}
	
	@Override
	public boolean isDirty() {
		return this.isDirty;
	}

	public void setDirty(boolean dirty) {
		this.isDirty = dirty;
		this.firePropertyChange(PROP_DIRTY);
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		super.doSave(monitor);
		setDirty(false);
	}
	
	public abstract void refresh();

}
