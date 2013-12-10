package eu.fittest.eclipse.component.junit.wizard;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;

import eu.fittest.eclipse.model.jobs.TestCase;

public class TestCaseEditingSupport extends EditingSupport {

	private TableViewer _viewer;
	
	public TestCaseEditingSupport(TableViewer viewer) {
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
		return ((TestCase)element).isSelected();
	}

	@Override
	protected void setValue(Object element, Object value) {
		((TestCase)element).setSelected((Boolean)value);
		_viewer.refresh();
	}

}
