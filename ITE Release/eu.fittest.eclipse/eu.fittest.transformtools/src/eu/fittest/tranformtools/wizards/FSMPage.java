package eu.fittest.tranformtools.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;


public class FSMPage extends WizardPage {
	private Text _outputModelFile;
	private Button _genDot;
	private String _outputFileName;
	
	public String getOutputModelFile() {
		return _outputModelFile.getText();
	}
	

	public boolean isGenDotFile() {
		return _genDot.getSelection();
	}

	protected FSMPage(String pageName, String outputFileName) {
		super(pageName);
		_outputFileName = outputFileName;
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		
		// Output folder
				
		// Output model file
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("Save inferred model to file: ");
  		_outputModelFile = new Text(composite, SWT.BORDER | SWT.SINGLE);
		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		_outputModelFile.setLayoutData(layoutData);		
		_outputModelFile.setText(_outputFileName);
		_outputModelFile.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		});
		// New project or output folder
		
		WUtil.createSpace(composite);
		
		label = new Label(composite, SWT.NONE);
		label.setText("Generate also a DOT file");
		
		_genDot = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		_genDot.setText("Yes");
		_genDot.setLayoutData(layoutData);
		_genDot.setSelection(true);
	
		setControl(composite);
		
		dialogChanged();
	}
	
	private void dialogChanged() {
		if(!_outputModelFile.getText().endsWith(".fsm")){
			updateStatus("Output file name must end with '.fsm'");
		}
		else {
			updateStatus(null);
		}
	}

	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
