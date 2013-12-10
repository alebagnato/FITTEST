package eu.fittest.eclipse.gui.preferences;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class FITTESTPage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public FITTESTPage() {
		super(FieldEditorPreferencePage.GRID);
	}

	@Override
	public void init(IWorkbench workbench) {
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Configure global ITE options");
	}

	@Override
	protected void createFieldEditors() {
		addField(new BooleanFieldEditor("ite.sut.agent.enabled", "Enable local mixed agent", getFieldEditorParent()));
	}
	
	@Override
	protected void performApply() {
		super.performApply();
		try {
			FITTESTServerStartUp.loadLocalSUTAgent();
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
	
	@Override
	public boolean performOk() {
		boolean result = super.performOk();
		try {
			FITTESTServerStartUp.loadLocalSUTAgent();
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			result = false;
		}
		return result;
	}

}
