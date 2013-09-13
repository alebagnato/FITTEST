package eu.fittest.tranformtools.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;


public class FSMPage extends WizardPage {

	private String inputFolder;
	private String outputModelFile;
	private boolean genDotFile = true;
	
	public String getInputFolder() {
		return inputFolder;
	}

	public String getOutputModelFile() {
		return outputModelFile;
	}
	
	

	public boolean isGenDotFile() {
		return genDotFile;
	}

	protected FSMPage(String pageName) {
		super(pageName);
		
		inputFolder = StorePreferences.getSavedValue("FSMPage_inputFolder");
		outputModelFile = StorePreferences.getSavedValue("FSMPage_outputModelFile");
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		// input folder
		
		// Output folder
		Label label = new Label(composite, SWT.NONE);
		label.setText("Input log folder:");
		
		final Text inputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		inputPath.setText(inputFolder);
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		inputPath.setLayoutData(layoutData);
		
		final Button inputFolderBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		inputFolderBrowse.setText("  Browse...  ");
		inputFolderBrowse.setLayoutData(layoutData);
		
		inputFolderBrowse.addMouseListener(new MouseListener(){

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				directoryDialog.setFilterPath(inputFolder);
				String tmp = directoryDialog.open();
				if (tmp != null){
					inputPath.setText(tmp);
					inputFolder = tmp;
					StorePreferences.saveValue("FSMPage_inputFolder", tmp);
				}
			}

			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);
		
		// Output model file
		
		label = new Label(composite, SWT.NONE);
		label.setText("Save inferred model to file: ");
  		final Text fsmFileText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fsmFileText.setText(outputModelFile);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fsmFileText.setLayoutData(layoutData);
		
		Button fsmFileBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		fsmFileBrowse.setText("  Browse..  ");
		fsmFileBrowse.setLayoutData(layoutData);
		
		fsmFileBrowse.addMouseListener(new MouseListener(){

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				
				FileDialog fileDialog = new FileDialog(getShell(), SWT.SAVE);
				String[] filters = {"*.fsm", "*.*"};
				fileDialog.setFilterExtensions(filters);
				fileDialog.setFilterPath(outputModelFile);
				String tmp = fileDialog.open();
				
//				String tmp = WUtil.getSelection(getShell(), "Model File", "Specify file to save inferred model");
				if (!tmp.equals("")){ 
					fsmFileText.setText(tmp);
					outputModelFile = tmp;
					StorePreferences.saveValue("FSMPage_outputModelFile", tmp);
				}
			}

			public void mouseUp(MouseEvent e) {
			}
			
		});
		WUtil.createSpace(composite);
		

		// New project or output folder
		label = new Label(composite, SWT.NONE);
		label.setText("Generate also a DOT file");
		
		final Button genDot = new Button(composite, SWT.CHECK);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		layoutData.horizontalSpan = 2;
		genDot.setText("Yes");
		genDot.setLayoutData(layoutData);
		genDot.setSelection(genDotFile);
		
		genDot.addMouseListener(new MouseListener() {
			
			
			public void mouseUp(MouseEvent e) {
				genDotFile = genDot.getSelection();
			}
			
			
			public void mouseDown(MouseEvent e) {
			}
			
			
			public void mouseDoubleClick(MouseEvent e) {
			}
		});
		
		
		setControl(composite);
	}

}
