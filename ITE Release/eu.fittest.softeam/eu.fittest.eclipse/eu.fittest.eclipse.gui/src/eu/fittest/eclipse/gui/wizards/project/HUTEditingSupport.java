package eu.fittest.eclipse.gui.wizards.project;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;

import eu.fittest.eclipse.model.environment.Host;

public class HUTEditingSupport extends EditingSupport {

	private TableViewer _viewer;
	
	public HUTEditingSupport(TableViewer viewer) {
		super(viewer);
		_viewer = viewer; 
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		return new CheckboxCellEditor(_viewer.getTable(), SWT.CHECK);
	}

	@Override
	protected boolean canEdit(Object element) {
		return true;
	}

	@Override
	protected Object getValue(Object element) {
		return ((Host)element).isHUT();
	}

	@Override
	protected void setValue(Object element, Object value) {
		((Host)element).setHUT((Boolean)value);
		_viewer.refresh();
	}

}
