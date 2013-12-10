package eu.fittest.eclipse.component;

import java.util.Collection;

import org.eclipse.jface.wizard.IWizardPage;

import eu.fittest.common.core.xml.Initialize.Parameter;

public interface IFITTESTComponentWizardPage extends IWizardPage{
	boolean isEnabled();
	Collection<Parameter> getInitializationParameters();
}
