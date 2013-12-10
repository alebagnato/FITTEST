package eu.fittest.eclipse.gui.wizards.project;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import eu.fittest.common.core.xml.HUTType;

import eu.fittest.eclipse.model.environment.Host;

public class HostTypeEditingSupport extends EditingSupport {

	private TableViewer _viewer;
	
	public HostTypeEditingSupport(TableViewer viewer) {
		super(viewer);
		_viewer = viewer; 
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		HUTType[] types = HUTType.values();
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
		switch (((Host)element).getType()) {
		case CLIENT:
			return 0;
		case SERVER:
		}
		return 1;
	}

	@Override
	protected void setValue(Object element, Object value) {
		Host host = (Host) element;
		switch ((Integer)value) {
		case 0:
			host.setType(HUTType.CLIENT);
			break;
		case 1:
			host.setType(HUTType.SERVER);
			break;
		}
		_viewer.refresh();
	}

}
