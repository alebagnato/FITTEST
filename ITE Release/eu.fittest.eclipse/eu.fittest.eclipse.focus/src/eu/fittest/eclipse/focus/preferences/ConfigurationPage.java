package eu.fittest.eclipse.focus.preferences;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import eu.fittest.eclipse.focus.Activator;

public class ConfigurationPage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public ConfigurationPage() {
		// TODO Auto-generated constructor stub
	}

	public ConfigurationPage(int style) {
		super(style);
		// TODO Auto-generated constructor stub
	}

	public ConfigurationPage(String title, int style) {
		super(title, style);
		// TODO Auto-generated constructor stub
	}

	public ConfigurationPage(String title, ImageDescriptor image, int style) {
		super(title, image, style);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void init(IWorkbench workbench) {
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Configure JDK in order to load IBM FoCuS tool");
	}

	@Override
	protected void createFieldEditors() {
		addField(new DirectoryFieldEditor("focus.pathtojdk", "Path to JDK", getFieldEditorParent()));

	}

}
