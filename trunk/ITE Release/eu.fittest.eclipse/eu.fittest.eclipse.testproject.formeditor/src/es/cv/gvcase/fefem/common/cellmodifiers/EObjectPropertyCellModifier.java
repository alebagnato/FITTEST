package es.cv.gvcase.fefem.common.cellmodifiers;

import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.SetCommand;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.forms.editor.FormPage;

import es.cv.gvcase.fefem.common.FEFEMPage;

public abstract class EObjectPropertyCellModifier implements ICellModifier {

	protected EditingDomain editingDomain;
	protected FEFEMPage page = null;
	
	public EObjectPropertyCellModifier(FormPage page, EditingDomain editingDomain){		
		if (page instanceof FEFEMPage){
			this.page = (FEFEMPage) page;
		}
		this.editingDomain = editingDomain;
	}
	
	public boolean canModify(Object element, String property) {
		return property.equals(this.getEObjectPropertyName(element));
	}

	public Object getValue(Object element, String property) {
		if (property.equals(this.getEObjectPropertyName(element))){
			return getEObjectPropertyValue(element);
		}
		return null;
	}

	public void modify(Object element, String property, Object value) {
		if(editingDomain == null) return;
		
		if(element instanceof TableItem) {
				if(property.equals(this.getEObjectPropertyName(element))) {
					Object currentValue = getEObjectPropertyValue(element);
					if(value != null && currentValue != null &&
							value.toString().equals(currentValue)) return;
					
					Command c = SetCommand.create(editingDomain, element,
							getEObjectPropertyFeature(), value);
					editingDomain.getCommandStack().execute(c);
					
					page.setDirty(true);
					
					page.refresh();
				}
		}

	}
	
	protected abstract String getEObjectPropertyName(Object element);
	protected abstract Object getEObjectPropertyValue(Object element);
	protected abstract EStructuralFeature getEObjectPropertyFeature();

}
