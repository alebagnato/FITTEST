/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.wizards;


import java.util.Arrays;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fbk.se.fsm.visitor.VisitorFactory;

public class XinputPage extends WizardPage {
	private String fsmModel;
	private String domainInputFile;
	private String logFolder;
	
	protected XinputPage(String pageName) {
		super(pageName);
		
		domainInputFile = StorePreferences.getSavedValue("XinputPage_domainInputFile");
		
		logFolder =  StorePreferences.getSavedValue("XinputPage_logFolder");
	}

	
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		composite.setLayout(layout);
		
		Label label = new Label(composite, SWT.NONE);
		label.setText("FSM model: ");
  		final Text fsmModelText = new Text(composite, SWT.BORDER | SWT.SINGLE);
		fsmModelText.setText(fsmModel);
		
		GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
		fsmModelText.setLayoutData(layoutData);
		
		Button fsmBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		fsmBrowse.setText("  Browse..  ");
		fsmBrowse.setLayoutData(layoutData);
		
		fsmBrowse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				
				String tmp = WUtil.getSelection(getShell(), "FSM Model", "Select a FSM model");
				if (!tmp.equals("")){ 
					fsmModelText.setText(tmp);
					fsmModel = tmp;
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);
		// Second row, output folder
		
		label = new Label(composite, SWT.NONE);
		label.setText("Log folder (optional):");
		final Text outputPath = new Text(composite, SWT.BORDER | SWT.SINGLE);
		outputPath.setText(logFolder);
		
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		outputPath.setLayoutData(layoutData);
		
		Button outputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		outputBrowse.setText("  Browse...  ");
		outputBrowse.setLayoutData(layoutData);
		
		outputBrowse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				/*
				DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				String tmp = directoryDialog.open();
				if (tmp != null){
					outputPath.setText(tmp);
					logFolder = tmp;
					
					if (!troposModel.equals("") && !logFolder.equals("")){
						setPageComplete(true);
					}
				}
				*/
				
				String tmp = WUtil.getSelection(getShell(), "Log Folder", "Select a folder that contains FITTEST logs");
				if (!tmp.equals("")){ 
					outputPath.setText(tmp);
					logFolder = tmp;
					
					StorePreferences.saveValue("XinputPage_logFolder", logFolder);
					
					if (!fsmModel.equals("") && !logFolder.equals("")){
						setPageComplete(true);
					}
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		WUtil.createSpace(composite);

		if (domainInputFile == null || domainInputFile == ""){
			domainInputFile = fsmModel.replace(".fsm", ".xml");
			StorePreferences.saveValue("XinputPage_domainInputFile", domainInputFile);
		}
		
		
		label = new Label(composite, SWT.NONE);
		label.setText("Domain input specification");
		
		final Text domainInputText = new Text(composite, SWT.BORDER);
		domainInputText.setText(domainInputFile);
		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		domainInputText.setLayoutData(layoutData);
		
		final Button domainInputBrowse = new Button(composite, SWT.NONE);
		layoutData = new GridData();
		layoutData.widthHint = 70;
		domainInputBrowse.setText("  Browse...  ");
		domainInputBrowse.setLayoutData(layoutData);
		
//		domainInputBrowse.setEnabled(false);
//		domainInputText.setEnabled(false);
		
		domainInputText.addKeyListener(new KeyListener() {
			
			@Override
			public void keyReleased(KeyEvent e) {
				domainInputFile = domainInputText.getText();
				StorePreferences.saveValue("XinputPage_domainInputFile", domainInputFile);
			}
			
			@Override
			public void keyPressed(KeyEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
		
		domainInputBrowse.addMouseListener(new MouseListener(){

			
			public void mouseDoubleClick(MouseEvent e) {
			}

			
			public void mouseDown(MouseEvent e) {
				
				//String tmp = getSelection("Scenario templates list", "Select the file that contains scenario templates");
				
				//DirectoryDialog directoryDialog = new DirectoryDialog(getShell());
				FileDialog fileDialog = new FileDialog(getShell());
				fileDialog.setFilterExtensions(new String[]{"*.xml"});
				fileDialog.setFileName(domainInputText.getText());
				fileDialog.setFilterPath(domainInputFile);
				String tmp = fileDialog.open();
				if (tmp != null){
					domainInputText.setText(tmp);
					domainInputFile = tmp;
					StorePreferences.saveValue("XinputPage_domainInputFile", tmp);
				}
			}

			
			public void mouseUp(MouseEvent e) {
			}
			
		});
		
		
		
		setControl(composite);
		return;
	}
	

	public String getFsmModel() {
		return fsmModel;
	}

	public void setFsmModel(String fsmModel) {
		this.fsmModel = fsmModel;
	}

	public String getDomainInputFile() {
		return domainInputFile;
	}

	public void setDomainInputFile(String domainInputFile) {
		this.domainInputFile = domainInputFile;
	}

	public String getOutputFolder() {
		return logFolder;
	}

	public void setOutputFolder(String outputFolder) {
		this.logFolder = outputFolder;
	}

	
}
