package eu.fittest.eclipse.gui.wizards.project;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import eu.fittest.common.core.xml.HUTEnvironment;

import eu.fittest.eclipse.model.environment.Host;

public class EnvironmentEditingSupport extends EditingSupport {

	private TableViewer _viewer;
	
	public EnvironmentEditingSupport(TableViewer viewer) {
		super(viewer);
		_viewer = viewer; 
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		HUTEnvironment[] types = HUTEnvironment.values();
		String[] values = new String[types.length];
		for(int i=0;i<types.length;i++){
			values[i] = types[i].name();
		}
		return new ComboBoxCellEditor(_viewer.getTable(), values, SWT.READ_ONLY);
	}

	@Override
	protected boolean canEdit(Object element) {
		return true;
	}

	@Override
	protected Object getValue(Object element) {
		switch (((Host)element).getEnvironment()) {
		case PRODUCTION:
			return 1;
		case TEST:
		}
		return 0;
	}

	@Override
	protected void setValue(Object element, Object value) {
		Host host = (Host) element;
		switch ((Integer)value) {
		case 0:
			host.setEnvironment(HUTEnvironment.TEST);
			break;
		case 1:
			host.setEnvironment(HUTEnvironment.PRODUCTION);
			break;
		}
		_viewer.refresh();
	}

}
