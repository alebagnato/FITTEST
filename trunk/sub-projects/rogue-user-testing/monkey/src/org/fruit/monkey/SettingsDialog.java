/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fruit.monkey;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.ToolTipManager;
import org.fruit.Pair;
import org.fruit.Util;

public class SettingsDialog extends javax.swing.JFrame {
	private static final long serialVersionUID = 5156320008281200950L;
	String settingsFile;
	Settings settings, ret;

	public SettingsDialog() throws IOException {
		try {
			for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
				if ("Nimbus".equals(info.getName())) {
					javax.swing.UIManager.setLookAndFeel(info.getClassName());
					break;
				}
			}
		} catch (ClassNotFoundException ex) {
			java.util.logging.Logger.getLogger(SettingsDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (InstantiationException ex) {
			java.util.logging.Logger.getLogger(SettingsDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (IllegalAccessException ex) {
			java.util.logging.Logger.getLogger(SettingsDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (javax.swing.UnsupportedLookAndFeelException ex) {
			java.util.logging.Logger.getLogger(SettingsDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		}

		ToolTipManager.sharedInstance().setDismissDelay(25000);
		ToolTipManager.sharedInstance().setInitialDelay(100);
		initComponents();
	}

	public Settings run(Settings settings, String settingsFile){
		this.settings = settings;
		this.settingsFile = settingsFile;
		this.ret = null;
		this.setVisible(true);
		populateInformation(settings);

		while(this.isShowing())
			Util.pause(0.1);

		extractInformation(settings);
		try {
			Util.saveToFile(settings.toFileString(), settingsFile);
		} catch (IOException e1) {
			Main.logln("Unable to save file: " + e1.toString());
		}

		return ret;
	}

	private Image loadIcon(String path) throws IOException{
		return ImageIO.read(this.getClass().getResourceAsStream(path));
	}

	private void start(AbstractProtocol.Modes mode){
		try{
			extractInformation(settings);
			checkSettings(settings);
			settings.set(ConfigTags.Mode, mode);
			ret = settings;
			this.dispose();
		}catch(IllegalStateException ise){
			JOptionPane.showMessageDialog(this, ise.getMessage(), "Invalid Settings!", JOptionPane.ERROR_MESSAGE);
		}
	}

	private void checkSettings(Settings settings) throws IllegalStateException{
		String userInputPattern = settings.get(ConfigTags.ProcessesToKillDuringTest);
		try {
			Pattern.compile(userInputPattern);
		} catch (PatternSyntaxException exception) {
			throw new IllegalStateException("Your ProcessFilter is not a valid regular expression!");
		}

		userInputPattern = settings.get(ConfigTags.ClickFilter);
		try {
			Pattern.compile(userInputPattern);
		} catch (PatternSyntaxException exception) {
			throw new IllegalStateException("Your ClickFilter is not a valid regular expression!");
		}

		userInputPattern = settings.get(ConfigTags.SuspiciousTitles);
		try {
			Pattern.compile(userInputPattern);
		} catch (PatternSyntaxException exception) {
			throw new IllegalStateException("Your Oracle is not a valid regular expression!");
		}

		if(! new File(settings.get(ConfigTags.OutputDir)).exists())
			throw new IllegalStateException("Output Directory does not exist!");
		if(! new File(settings.get(ConfigTags.TempDir)).exists())
			throw new IllegalStateException("Temp Directory does not exist!");


		for(int i = 0; i < tblCopyFromTo.getRowCount(); i++){
			String left = (String) tblCopyFromTo.getValueAt(i, 0);
			String right = (String) tblCopyFromTo.getValueAt(i, 1);

			if(left != null || right != null){
				if((left != null && right == null) ||
						(left == null && right != null) ||
						left.trim().equals("") ||
						right.trim().equals(""))
					throw new IllegalStateException("CopyFromTo Table has unfinished entries!");
			}
		}		
	}


	public void populateInformation(Settings settings){
		checkStopOnFault.setSelected(settings.get(ConfigTags.StopGenerationOnFault));
		checkUseRecordedTimes.setSelected(settings.get(ConfigTags.UseRecordedActionDurationAndWaitTimeDuringReplay));
		txtSutPath.setText(settings.get(ConfigTags.Executable));
		spnActionWaitTime.setValue(settings.get(ConfigTags.TimeToWaitAfterAction));
		spnActionDuration.setValue(settings.get(ConfigTags.ActionDuration));
		spnSutStartupTime.setValue(settings.get(ConfigTags.StartupTime));
		txtSuspTitles.setText(settings.get(ConfigTags.SuspiciousTitles));
		txtClickFilter.setText(settings.get(ConfigTags.ClickFilter));
		txtProcessFilter.setText(settings.get(ConfigTags.ProcessesToKillDuringTest));
		spnNumSequences.setValue(settings.get(ConfigTags.Sequences));
		spnSequenceLength.setValue(settings.get(ConfigTags.SequenceLength));
		checkForceForeground.setSelected(settings.get(ConfigTags.ForceForeground));
		spnVerbosity.setValue(settings.get(ConfigTags.LogLevel));
		spnMaxTime.setValue(settings.get(ConfigTags.MaxTime));
		txtOutputDir.setText(settings.get(ConfigTags.OutputDir));
		txtTempDir.setText(settings.get(ConfigTags.TempDir));
		spnFreezeTime.setValue(settings.get(ConfigTags.TimeToFreeze));

		for(int i = 0; i < tblCopyFromTo.getRowCount(); i++){
			tblCopyFromTo.setValueAt(null, i, 0);
			tblCopyFromTo.setValueAt(null, i, 1);
		}

		int i = 0;
		for(Pair<String, String> fromTo : settings.get(ConfigTags.CopyFromTo)){
			tblCopyFromTo.setValueAt(fromTo.left(), i, 0);
			tblCopyFromTo.setValueAt(fromTo.right(), i, 1);
			i++;
		}

		for(i = 0; i < tblDelete.getRowCount(); i++)
			tblDelete.setValueAt(null, i, 0);

		i = 0;
		for(String f : settings.get(ConfigTags.Delete)){
			tblDelete.setValueAt(f, i, 0);
			i++;
		}
	}

	public void extractInformation(Settings settings){
		settings.set(ConfigTags.StopGenerationOnFault, checkStopOnFault.isSelected());
		settings.set(ConfigTags.UseRecordedActionDurationAndWaitTimeDuringReplay, checkUseRecordedTimes.isSelected());
		settings.set(ConfigTags.Executable, txtSutPath.getText());
		settings.set(ConfigTags.ActionDuration, (Double)spnActionDuration.getValue());
		settings.set(ConfigTags.TimeToWaitAfterAction, (Double)spnActionWaitTime.getValue());
		settings.set(ConfigTags.StartupTime, (Double)spnSutStartupTime.getValue());
		settings.set(ConfigTags.SuspiciousTitles, txtSuspTitles.getText());
		settings.set(ConfigTags.ClickFilter, txtClickFilter.getText());
		settings.set(ConfigTags.ProcessesToKillDuringTest, txtProcessFilter.getText()); 
		settings.set(ConfigTags.Sequences, (Integer)spnNumSequences.getValue()); 
		settings.set(ConfigTags.LogLevel, (Integer)spnVerbosity.getValue()); 
		settings.set(ConfigTags.SequenceLength, (Integer)spnSequenceLength.getValue());
		settings.set(ConfigTags.ForceForeground, checkForceForeground.isSelected());
		settings.set(ConfigTags.MaxTime, (Double) spnMaxTime.getValue());
		settings.set(ConfigTags.OutputDir, txtOutputDir.getText()); 
		settings.set(ConfigTags.TempDir, txtTempDir.getText()); 
		settings.set(ConfigTags.TimeToFreeze, (Double)spnFreezeTime.getValue());

		List<Pair<String, String>> copyFromTo = Util.newArrayList();
		for(int i = 0; i < tblCopyFromTo.getRowCount(); i++){
			String left = (String) tblCopyFromTo.getValueAt(i, 0);
			String right = (String) tblCopyFromTo.getValueAt(i, 1);

			if(left != null && right != null)
				copyFromTo.add(Pair.from(left, right)); 
		}
		settings.set(ConfigTags.CopyFromTo, copyFromTo);

		List<String> delete = Util.newArrayList();
		for(int i = 0; i < tblDelete.getRowCount(); i++){
			String value = (String) tblDelete.getValueAt(i, 0);			
			if(value != null)
				delete.add(value); 
		}
		settings.set(ConfigTags.Delete, delete);
	}


	private void initComponents() throws IOException {

		jButton1 = new javax.swing.JButton();
		btnGenerate = new javax.swing.JButton();
		btnSpy = new javax.swing.JButton();
		jTabbedPane1 = new javax.swing.JTabbedPane();
		aboutPanel = new javax.swing.JPanel();
		jPanel1 = new javax.swing.JPanel();
		txtSutPath = new javax.swing.JTextField();
		jLabel1 = new javax.swing.JLabel();
		btnSutPath = new javax.swing.JButton();
		jLabel15 = new javax.swing.JLabel();
		spnNumSequences = new javax.swing.JSpinner();
		jLabel17 = new javax.swing.JLabel();
		spnSequenceLength = new javax.swing.JSpinner();
		jLabel18 = new javax.swing.JLabel();
		jLabel19 = new javax.swing.JLabel();
		checkForceForeground = new javax.swing.JCheckBox();
		btnLoadSettings = new javax.swing.JButton();
		btnSaveSettingsAs = new javax.swing.JButton();
		jLabel21 = new javax.swing.JLabel();
		checkStopOnFault = new javax.swing.JCheckBox();
		jLabel25 = new javax.swing.JLabel();
		spnVerbosity = new javax.swing.JSpinner();
		jPanel4 = new javax.swing.JPanel();
		jLabel10 = new javax.swing.JLabel();
		jScrollPane2 = new javax.swing.JScrollPane();
		txtSuspTitles = new javax.swing.JTextArea();
		jLabel13 = new javax.swing.JLabel();
		spnFreezeTime = new javax.swing.JSpinner();
		jLabel14 = new javax.swing.JLabel();
		jPanel5 = new javax.swing.JPanel();
		jLabel11 = new javax.swing.JLabel();
		jScrollPane1 = new javax.swing.JScrollPane();
		txtClickFilter = new javax.swing.JTextArea();
		jScrollPane3 = new javax.swing.JScrollPane();
		txtProcessFilter = new javax.swing.JTextArea();
		jLabel12 = new javax.swing.JLabel();
		jPanel2 = new javax.swing.JPanel();
		spnActionDuration = new javax.swing.JSpinner();
		jLabel2 = new javax.swing.JLabel();
		jLabel3 = new javax.swing.JLabel();
		jLabel26 = new javax.swing.JLabel();
		jLabel27 = new javax.swing.JLabel();
		jLabel4 = new javax.swing.JLabel();
		spnActionWaitTime = new javax.swing.JSpinner();
		jLabel5 = new javax.swing.JLabel();
		jLabel6 = new javax.swing.JLabel();
		spnSutStartupTime = new javax.swing.JSpinner();
		jLabel7 = new javax.swing.JLabel();
		jLabel22 = new javax.swing.JLabel();
		jLabel28 = new javax.swing.JLabel();
		spnMaxTime = new javax.swing.JSpinner();
		jLabel23 = new javax.swing.JLabel();
		jLabel24 = new javax.swing.JLabel();
		checkUseRecordedTimes = new javax.swing.JCheckBox();
		jPanel3 = new javax.swing.JPanel();
		jLabel8 = new javax.swing.JLabel();
		txtOutputDir = new javax.swing.JTextField();
		btnSetOutputDir = new javax.swing.JButton();
		jLabel9 = new javax.swing.JLabel();
		txtTempDir = new javax.swing.JTextField();
		btnSetTempDir = new javax.swing.JButton();
		jLabel16 = new javax.swing.JLabel();
		jScrollPane5 = new javax.swing.JScrollPane();
		tblCopyFromTo = new javax.swing.JTable();
		jLabel20 = new javax.swing.JLabel();
		jScrollPane4 = new javax.swing.JScrollPane();
		tblDelete = new javax.swing.JTable();
		btnReplay = new javax.swing.JButton();
		btnView = new javax.swing.JButton();
		btnEditProtocol = new javax.swing.JButton("Edit Protocol");

		btnEditProtocol.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnEditProtocolActionPerformed(evt);
			}
		});

		btnEditProtocol.setToolTipText("Edit the protocol");
		btnEditProtocol.setMaximumSize(new java.awt.Dimension(160, 35));
		btnEditProtocol.setMinimumSize(new java.awt.Dimension(160, 35));
		btnEditProtocol.setPreferredSize(new java.awt.Dimension(160, 35));


		jButton1.setText("jButton1");
		jButton1.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jButton1ActionPerformed(evt);
			}
		});

		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setTitle("Rogue User Settings");
		setLocationByPlatform(true);
		setName("Rogue User Settings"); // NOI18N
		setResizable(false);

		btnGenerate.setBackground(new java.awt.Color(255, 255, 255));
		btnGenerate.setIcon(new ImageIcon(loadIcon("/resources/icons/engine.jpg")));
		btnGenerate.setToolTipText("<html>\nStart in Generation-Mode:<br>\nThis mode will start the SUT and execute a full test.\n</html>");
		btnGenerate.setFocusPainted(false);
		btnGenerate.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnGenerateActionPerformed(evt);
			}
		});

		btnSpy.setBackground(new java.awt.Color(255, 255, 255));
		btnSpy.setIcon(new ImageIcon(loadIcon("/resources/icons/magnifier.png")));
		btnSpy.setToolTipText("<html>\nStart in Spy-Mode: <br>\nThis mode does allows you to inspect the GUI of the System under Test. <br>\nSimply use the mouse cursor to point on a widget and the Rogue User<br>\nwill display everything it knows about it. The Spy-Mode will also visualize<br>\nthe set of actions that the Rogue User recognizes, so that you can see<br>\nwhich ones will be executed during a test.\n</html>");
		btnSpy.setFocusPainted(false);
		btnSpy.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnSpyActionPerformed(evt);
			}
		});

		jTabbedPane1.setName("");

		txtSutPath.setToolTipText("<html> Path to the SUT: Pick the executable of the SUT or insert a custom command line. </html>");

		jLabel1.setText("Path to SUT:");
		jLabel1.setToolTipText("<html>\nPath to the SUT: Pick the executable of the SUT or insert a custom command line.\n</html>");

		btnSutPath.setText("...");
		btnSutPath.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnSutPathActionPerformed(evt);
			}
		});

		jLabel15.setText("Number of Sequences:");
		jLabel15.setToolTipText("<html>\nNumber of sequences to generate.\n</html>");

		spnNumSequences.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(1), Integer.valueOf(1), null, Integer.valueOf(1)));
		spnNumSequences.setToolTipText("<html> Number of sequences to generate. </html>");

		jLabel17.setText("Sequence Length:");
		jLabel17.setToolTipText("<html>\nSequence length: After having executed the given amount of<br>\nactions, the RU will stop the SUT and proceed with the next<br>\nsequence.\n</html>");

		spnSequenceLength.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(1), Integer.valueOf(1), null, Integer.valueOf(1)));
		spnSequenceLength.setToolTipText("<html> Sequence length: After having executed the given amount of<br> actions, the RU will stop the SUT and proceed with the next<br> sequence. </html>");

		jLabel18.setText("actions");
		jLabel18.setToolTipText("<html> Sequence length: After having executed the given amount of<br> actions, the RU will stop the SUT and proceed with the next<br> sequence. </html>");

		jLabel19.setText("Force SUT to Foreground:");
		jLabel19.setToolTipText("<html>\nForce the SUT to the foreground: During test generation, windows might get minimized or other<br>\n processes might block the SUT's GUI. If you check this option, the RU will force the SUT to the<br>\nforeground.\n</html>");

		checkForceForeground.setToolTipText("<html> Force the SUT to the foreground: During test generation, windows might get minimized or other<br>  processes might block the SUT's GUI. If you check this option, the RU will force the SUT to the<br> foreground. </html>");

		btnLoadSettings.setIcon(new ImageIcon(loadIcon("/resources/icons/open.jpg")));
		btnLoadSettings.setToolTipText("Load a settings file");
		btnLoadSettings.setMaximumSize(new java.awt.Dimension(35, 35));
		btnLoadSettings.setMinimumSize(new java.awt.Dimension(35, 35));
		btnLoadSettings.setPreferredSize(new java.awt.Dimension(35, 35));
		btnLoadSettings.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnLoadSettingsActionPerformed(evt);
			}
		});

		btnSaveSettingsAs.setIcon(new ImageIcon(loadIcon("/resources/icons/save.jpg")));
		btnSaveSettingsAs.setToolTipText("Save current settings to file");
		btnSaveSettingsAs.setMaximumSize(new java.awt.Dimension(35, 35));
		btnSaveSettingsAs.setMinimumSize(new java.awt.Dimension(35, 35));
		btnSaveSettingsAs.setPreferredSize(new java.awt.Dimension(35, 35));
		btnSaveSettingsAs.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnSaveSettingsAsActionPerformed(evt);
			}
		});

		jLabel21.setText("Stop Generation on Fault:");
		jLabel21.setToolTipText("<html>\nStop sequence generation on fault: If the RU detects and error, it will immediately stop sequence generation.\n</html>");

		checkStopOnFault.setToolTipText("<html> Stop sequence generation on fault: If the RU detects and error, it will immediately stop sequence generation. </html>");

		jLabel25.setText("Logging Verbosity:");
		jLabel25.setToolTipText("<html>\nLogging verbosity: The higher the value, the more information<br>\nwill be written to the RU's log-file.\n</html>");

		spnVerbosity.setModel(new javax.swing.SpinnerNumberModel(0, 0, 2, 1));
		spnVerbosity.setToolTipText("<html> Logging verbosity: The higher the value, the more information<br> will be written to the RU's log-file. </html>");

		javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
		jPanel1.setLayout(jPanel1Layout);
		jPanel1Layout.setHorizontalGroup(
				jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel1Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
										.addGap(0, 0, Short.MAX_VALUE)
										.addComponent(btnEditProtocol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(btnLoadSettings, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(btnSaveSettingsAs, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addGroup(jPanel1Layout.createSequentialGroup()
												.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
														.addGroup(jPanel1Layout.createSequentialGroup()
																.addComponent(jLabel1)
																.addGap(18, 18, 18)
																.addComponent(txtSutPath, javax.swing.GroupLayout.PREFERRED_SIZE, 377, javax.swing.GroupLayout.PREFERRED_SIZE)
																.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																.addComponent(btnSutPath, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE))
																.addGroup(jPanel1Layout.createSequentialGroup()
																		.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																				.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
																						.addComponent(jLabel21, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
																						.addComponent(jLabel15, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
																						.addComponent(jLabel17, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
																						.addComponent(jLabel19, javax.swing.GroupLayout.DEFAULT_SIZE, 164, Short.MAX_VALUE))
																						.addComponent(jLabel25, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE))
																						.addGap(37, 37, 37)
																						.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																								.addComponent(spnVerbosity, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE)
																								.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																										.addComponent(spnNumSequences, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE)
																										.addGroup(jPanel1Layout.createSequentialGroup()
																												.addComponent(spnSequenceLength, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE)
																												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
																												.addComponent(jLabel18, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE))
																												.addComponent(checkForceForeground)
																												.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
																														.addComponent(checkStopOnFault)
																														.addGap(264, 264, 264))))))
																														.addGap(0, 0, Short.MAX_VALUE)))
																														.addContainerGap())
				);
		jPanel1Layout.setVerticalGroup(
				jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel1Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								.addComponent(txtSutPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel1)
								.addComponent(btnSutPath, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
								.addGap(28, 28, 28)
								.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(spnNumSequences, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jLabel15))
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
												.addComponent(spnSequenceLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addComponent(jLabel17)
												.addComponent(jLabel18))
												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
												.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
														.addComponent(jLabel19)
														.addComponent(checkForceForeground))
														.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
														.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																.addComponent(jLabel21)
																.addComponent(checkStopOnFault))
																.addGap(18, 18, 18)
																.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																		.addComponent(spnVerbosity, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
																		.addComponent(jLabel25))
																		.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 114, Short.MAX_VALUE)
																		.addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
																				.addComponent(btnSaveSettingsAs, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
																				.addComponent(btnLoadSettings, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
																				.addComponent(btnEditProtocol, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
				);

		

        jLabel26.setIcon(new ImageIcon(loadIcon("/resources/icons/fittest_logo.png")));
        jLabel27.setIcon(new ImageIcon(loadIcon("/resources/icons/fp7_logo.png")));

        jLabel28.setFont(new java.awt.Font("Tahoma", 1, 14)); // NOI18N
        jLabel28.setText("Rogue User v1.0");

        javax.swing.GroupLayout aboutPanelLayout = new javax.swing.GroupLayout(aboutPanel);
        aboutPanel.setLayout(aboutPanelLayout);
        aboutPanelLayout.setHorizontalGroup(
            aboutPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(aboutPanelLayout.createSequentialGroup()
                .addGroup(aboutPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(aboutPanelLayout.createSequentialGroup()
                        .addGap(88, 88, 88)
                        .addComponent(jLabel27, javax.swing.GroupLayout.PREFERRED_SIZE, 140, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(18, 18, 18)
                        .addComponent(jLabel26, javax.swing.GroupLayout.PREFERRED_SIZE, 210, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(aboutPanelLayout.createSequentialGroup()
                        .addGap(177, 177, 177)
                        .addComponent(jLabel28, javax.swing.GroupLayout.PREFERRED_SIZE, 124, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(44, Short.MAX_VALUE))
        );
        aboutPanelLayout.setVerticalGroup(
            aboutPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(aboutPanelLayout.createSequentialGroup()
                .addGap(22, 22, 22)
                .addComponent(jLabel28, javax.swing.GroupLayout.PREFERRED_SIZE, 44, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(aboutPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(aboutPanelLayout.createSequentialGroup()
                        .addGap(33, 33, 33)
                        .addComponent(jLabel27, javax.swing.GroupLayout.PREFERRED_SIZE, 140, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(aboutPanelLayout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jLabel26, javax.swing.GroupLayout.PREFERRED_SIZE, 178, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(92, Short.MAX_VALUE))
        );

        
		jTabbedPane1.addTab("About", aboutPanel);

		jTabbedPane1.addTab("General Settings", jPanel1);

		jLabel10.setText("Suspicious Titles:");
		jLabel10.setToolTipText("<html>\nThis is a very simple oracle in the form of a regular expression, which is applied to each<br>widget's Title property. If the RU finds a widget on the screen, whose title matches the given<br>\nexpression, it will consider the current sequence to be erroneous.\n</html>");

		txtSuspTitles.setColumns(20);
		txtSuspTitles.setLineWrap(true);
		txtSuspTitles.setRows(5);
		txtSuspTitles.setToolTipText("<html> This is a very simple oracle in the form of a regular expression, which is applied to each<br> widget's Title property. If the RU finds a widget on the screen, whose title matches the given<br> expression, it will consider the current sequence to be erroneous. </html>");
		jScrollPane2.setViewportView(txtSuspTitles);

		jLabel13.setText("Freeze Time:");

		spnFreezeTime.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(1.0d), Double.valueOf(1.0d), null, Double.valueOf(1.0d)));

		jLabel14.setText("seconds");

		javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
		jPanel4.setLayout(jPanel4Layout);
		jPanel4Layout.setHorizontalGroup(
				jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel4Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addGroup(jPanel4Layout.createSequentialGroup()
										.addComponent(jLabel10, javax.swing.GroupLayout.PREFERRED_SIZE, 142, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 341, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addGroup(jPanel4Layout.createSequentialGroup()
												.addComponent(jLabel13, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addGap(18, 18, 18)
												.addComponent(spnFreezeTime, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
												.addComponent(jLabel14)))
												.addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
				);
		jPanel4Layout.setVerticalGroup(
				jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel4Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel10))
								.addGap(32, 32, 32)
								.addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(spnFreezeTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jLabel13)
										.addComponent(jLabel14))
										.addContainerGap(190, Short.MAX_VALUE))
				);

		jTabbedPane1.addTab("Oracle", jPanel4);

		jLabel11.setText("Click Filter:");
		jLabel11.setToolTipText("<html>\nClick-filter: Certain actions that the Rogue Users wants to execute might be dangerous or<br>\nundesirable, such as printing out documents, creating, moving or deleting files. The Rogue<br>\nUser will not execute clicks on any widget whose title matches the given regular expression.<br>\nTo see whether or not your expression works, simply start the RU in Spy-Mode, which will<br>\nvisualize the detected actions.\n</html>");

		txtClickFilter.setColumns(20);
		txtClickFilter.setLineWrap(true);
		txtClickFilter.setRows(5);
		txtClickFilter.setToolTipText("<html> Click-filter: Certain actions that the Rogue Users wants to execute might be dangerous or<br> undesirable, such as printing out documents, creating, moving or deleting files. The Rogue<br> User will not execute clicks on any widget whose title matches the given regular expression.<br> To see whether or not your expression works, simply start the RU in Spy-Mode, which will<br> visualize the detected actions. </html>");
		jScrollPane1.setViewportView(txtClickFilter);

		txtProcessFilter.setColumns(20);
		txtProcessFilter.setLineWrap(true);
		txtProcessFilter.setRows(5);
		txtProcessFilter.setToolTipText("<html> Processes to kill: Some SUTs start other processes during test sequence generation. These might<br> popup in the foreground and block the SUTs GUI. They might also consume excessive memory, etc.<br> The RU will kill any process whose name matches the given regular expression. </html>");
		jScrollPane3.setViewportView(txtProcessFilter);

		jLabel12.setText("Process Filter:");
		jLabel12.setToolTipText("<html>\nProcesses to kill: Some SUTs start other processes during test sequence generation. These might<br>\npopup in the foreground and block the SUTs GUI. They might also consume excessive memory, etc.<br>\nThe RU will kill any process whose name matches the given regular expression.\n</html>");

		javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
		jPanel5.setLayout(jPanel5Layout);
		jPanel5Layout.setHorizontalGroup(
				jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel5Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
								.addComponent(jLabel12, javax.swing.GroupLayout.PREFERRED_SIZE, 90, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel11, javax.swing.GroupLayout.DEFAULT_SIZE, 96, Short.MAX_VALUE))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
										.addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 384, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 384, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
				);
		jPanel5Layout.setVerticalGroup(
				jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel5Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel11))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
								.addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
										.addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jLabel12))
										.addContainerGap(133, Short.MAX_VALUE))
				);

		jTabbedPane1.addTab("Filters", jPanel5);

		spnActionDuration.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
		spnActionDuration.setToolTipText("<html> Action Duration: The higher this value, the longer the execution of actions will take.<br> Mouse movements and typing become slower, so that it is easier to follow what the<br> Rogue User is doing. This can be useful during Replay-Mode, in order to replay<br> a recorded sequence with less speed to better understand a fault. </html>");

		jLabel2.setText("Action Duration:");
		jLabel2.setToolTipText("<html>\nAction Duration: The higher this value, the longer the execution of actions will take.<br>\nMouse movements and typing become slower, so that it is easier to follow what the<br>\nRogue User is doing. This can be useful during Replay-Mode, in order to replay<br>\na recorded sequence with less speed to better understand a fault.\n</html>");

		jLabel3.setText("seconds");
		jLabel3.setToolTipText("<html> Action Duration: The higher this value, the longer the execution of actions will take.<br> Mouse movements and typing become slower, so that it is easier to follow what the<br> Rogue User is doing. This can be useful during Replay-Mode, in order to replay<br> a recorded sequence with less speed to better understand a fault. </html>");

		jLabel4.setText("Action Wait Time:");
		jLabel4.setToolTipText("<html>\nTime to wait after execution of an action: This is the time that the Rogue User<br>\npauses after having executed an action in Generation-Mode. Usually, this value<br>\nis set to 0. However, sometimes it can make sense to give the GUI of the SUT<br>\nmore time to react, before executing the next action. If this value is set to a<br>\nvalue > 0, it can greatly enhance reproducibility of sequences at the expense<br>\nof longer testing times.\n</html>");

		spnActionWaitTime.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(0.0d), null, Double.valueOf(0.1d)));
		spnActionWaitTime.setToolTipText("<html> Time to wait after execution of an action: This is the time that the Rogue User<br> pauses after having executed an action in Generation-Mode. Usually, this value<br> is set to 0. However, sometimes it can make sense to give the GUI of the SUT<br> more time to react, before executing the next action. If this value is set to a<br> value > 0, it can greatly enhance reproducibility of sequences at the expense<br> of longer testing times. </html>");

		jLabel5.setText("seconds");
		jLabel5.setToolTipText("<html> Time to wait after execution of an action: This is the time that the Rogue User<br> pauses after having executed an action in Generation-Mode. Usually, this value<br> is set to 0. However, sometimes it can make sense to give the GUI of the SUT<br> more time to react, before executing the next action. If this value is set to a<br> value > 0, it can greatly enhance reproducibility of sequences at the expense<br> of longer testing times. </html>");

		jLabel6.setText("SUT Startup Time:");
		jLabel6.setToolTipText("<html>\nSUT startup time: This is the time that the Rogue User waits for the SUT to load. Large and<br>\n complex SUTs might need more time than small ones. Only after this time has expired, the<br>\nRogue User will start sequence generation.\n</html>");

		spnSutStartupTime.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(0.0d), null, Double.valueOf(1.0d)));
		spnSutStartupTime.setToolTipText("<html> SUT startup time: This is the time that the Rogue User waits for the SUT to load. Large and<br>  complex SUTs might need more time than small ones. Only after this time has expired, the<br> Rogue User will start sequence generation. </html>");

		jLabel7.setText("seconds");
		jLabel7.setToolTipText("<html>SUT startup time: This is the time that the Rogue User waits for the SUT to load. Large and<br>  complex SUTs might need more time than small ones. Only after this time has expired, the<br> Rogue User will start sequence generation. </html>");

		jLabel22.setText("Max. Test Time:");
		jLabel22.setToolTipText("<html>\nMaximum test time (seconds): The RU will cease to generate any sequences after this time has elapsed.<br>\nThis is useful for specifying a test time out, e.g. one hour, one day, one week.\n</html>");

		spnMaxTime.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(0.0d), null, Double.valueOf(1.0d)));
		spnMaxTime.setToolTipText("<html> Maximum test time (seconds): The RU will cease to generate any sequences after this time has elapsed.<br> This is useful for specifying a test time out, e.g. one hour, one day, one week. </html>");

		jLabel23.setText("seconds");
		jLabel23.setToolTipText("<html> Maximum test time (seconds): The RU will cease to generate any sequences after this time has elapsed.<br> This is useful for specifying a test time out, e.g. e.g. one hour, one day, one week. </html>");

		jLabel24.setText("Use Recorded Action Timing during Replay:");
		jLabel24.setToolTipText("<html>\nThis option only affects Replay-Mode. If checked, the RU will use the action duration and action<br>\nwait time that was used during sequence generation. If you uncheck the option, you can specify<br>\nyour own values.\n</html>");

		checkUseRecordedTimes.setToolTipText("<html> This option only affects Replay-Mode. If checked, the RU will use the action duration and action<br> wait time that was used during sequence generation. If you uncheck the option, you can specify<br> your own values. </html>");

		javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
		jPanel2.setLayout(jPanel2Layout);
		jPanel2Layout.setHorizontalGroup(
				jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel2Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
										.addGroup(jPanel2Layout.createSequentialGroup()
												.addComponent(jLabel6, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addGap(41, 41, 41)
												.addComponent(spnSutStartupTime, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
												.addComponent(jLabel7))
												.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
														.addGroup(jPanel2Layout.createSequentialGroup()
																.addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
																.addGap(41, 41, 41)
																.addComponent(spnActionWaitTime, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
																.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																.addComponent(jLabel5))
																.addGroup(jPanel2Layout.createSequentialGroup()
																		.addComponent(jLabel2, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
																		.addGap(41, 41, 41)
																		.addComponent(spnActionDuration, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
																		.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																		.addComponent(jLabel3))))
																		.addGroup(jPanel2Layout.createSequentialGroup()
																				.addComponent(jLabel22, javax.swing.GroupLayout.PREFERRED_SIZE, 130, javax.swing.GroupLayout.PREFERRED_SIZE)
																				.addGap(41, 41, 41)
																				.addComponent(spnMaxTime, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
																				.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																				.addComponent(jLabel23))
																				.addGroup(jPanel2Layout.createSequentialGroup()
																						.addComponent(jLabel24, javax.swing.GroupLayout.PREFERRED_SIZE, 264, javax.swing.GroupLayout.PREFERRED_SIZE)
																						.addGap(18, 18, 18)
																						.addComponent(checkUseRecordedTimes)))
																						.addContainerGap(187, Short.MAX_VALUE))
				);
		jPanel2Layout.setVerticalGroup(
				jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel2Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								.addComponent(spnActionDuration, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel2)
								.addComponent(jLabel3))
								.addGap(18, 18, 18)
								.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(spnActionWaitTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jLabel4)
										.addComponent(jLabel5))
										.addGap(18, 18, 18)
										.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
												.addComponent(spnSutStartupTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
												.addComponent(jLabel6)
												.addComponent(jLabel7))
												.addGap(18, 18, 18)
												.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
														.addComponent(spnMaxTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
														.addComponent(jLabel22)
														.addComponent(jLabel23))
														.addGap(18, 18, 18)
														.addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																.addComponent(jLabel24)
																.addComponent(checkUseRecordedTimes))
																.addContainerGap(171, Short.MAX_VALUE))
				);

		jTabbedPane1.addTab("Time Settings", jPanel2);

		jLabel8.setText("Output Directory:");

		btnSetOutputDir.setText("...");
		btnSetOutputDir.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnSetOutputDirActionPerformed(evt);
			}
		});

		jLabel9.setText("Temp Directory:");

		btnSetTempDir.setText("...");
		btnSetTempDir.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnSetTempDirActionPerformed(evt);
			}
		});

		jLabel16.setText("Copy Files on SUT Startup:");
		jLabel16.setToolTipText("<html> Files to copy before SUT start. It is useful to restore certain<br>  configuration files to their default. Therefore you can define pairs of paths (copy from / to).<br> The RU will copy each specified file from the given source location to the given destination.<br> Simply double-click the text-area and a file dialog will pop up. </html>");

		tblCopyFromTo.setModel(new javax.swing.table.DefaultTableModel(
				new Object [][] {
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null},
						{null, null}
				},
				new String [] {
						"Source File / Directory", "Destination"
				}
				) {
			private static final long serialVersionUID = 1L;
			Class<?>[] types = new Class<?> [] {
					java.lang.String.class, java.lang.String.class
			};

			public Class<?> getColumnClass(int columnIndex) {
				return types [columnIndex];
			}
		});
		tblCopyFromTo.setToolTipText("<html>\nFiles to copy before SUT start. It is useful to restore certain<br>\n configuration files to their default. Therefore you can define pairs of paths (copy from / to).<br>\nThe RU will copy each specified file from the given source location to the given destination.<br>\nSimply double-click the text-area and a file dialog will pop up.\n</html>");
		tblCopyFromTo.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				tblCopyFromToMouseClicked(evt);
			}
		});
		jScrollPane5.setViewportView(tblCopyFromTo);

		jLabel20.setText("Delete Files on SUT Startup:");
		jLabel20.setToolTipText("<html> Files to delete before SUT start: Certain SUTs generate configuration files, temporary files and files<br> that save the system's state. This might be problematic during sequence replay, when you want a<br> system to always start in the same state. Therefore, you can specify these files, to be deleted<br> before the SUT gets started. If you double-click the text-area a file dialog will pop up which allows<br> selecting files and directories to be deleted. </html>");

		tblDelete.setModel(new javax.swing.table.DefaultTableModel(
				new Object [][] {
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null},
						{null}
				},
				new String [] {
						"File / Directory"
				}
				) {
			private static final long serialVersionUID = 1L;
			Class<?>[] types = new Class<?> [] {
					java.lang.String.class
			};

			public Class<?> getColumnClass(int columnIndex) {
				return types [columnIndex];
			}
		});
		tblDelete.setToolTipText("<html>\nFiles to delete before SUT start: Certain SUTs generate configuration files, temporary files and files<br>\nthat save the system's state. This might be problematic during sequence replay, when you want a<br>\nsystem to always start in the same state. Therefore, you can specify these files, to be deleted<br>\nbefore the SUT gets started. If you double-click the text-area a file dialog will pop up which allows<br>\nselecting files and directories to be deleted.\n</html>");
		tblDelete.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				tblDeleteMouseClicked(evt);
			}
		});
		jScrollPane4.setViewportView(tblDelete);

		javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
		jPanel3.setLayout(jPanel3Layout);
		jPanel3Layout.setHorizontalGroup(
				jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel3Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addComponent(jScrollPane5)
								.addComponent(jScrollPane4)
								.addGroup(jPanel3Layout.createSequentialGroup()
										.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
												.addGroup(jPanel3Layout.createSequentialGroup()
														.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
																.addGroup(jPanel3Layout.createSequentialGroup()
																		.addComponent(jLabel8, javax.swing.GroupLayout.PREFERRED_SIZE, 92, javax.swing.GroupLayout.PREFERRED_SIZE)
																		.addGap(18, 18, 18)
																		.addComponent(txtOutputDir))
																		.addGroup(jPanel3Layout.createSequentialGroup()
																				.addComponent(jLabel9, javax.swing.GroupLayout.PREFERRED_SIZE, 92, javax.swing.GroupLayout.PREFERRED_SIZE)
																				.addGap(18, 18, 18)
																				.addComponent(txtTempDir, javax.swing.GroupLayout.PREFERRED_SIZE, 346, javax.swing.GroupLayout.PREFERRED_SIZE))
																				.addComponent(jLabel16, javax.swing.GroupLayout.PREFERRED_SIZE, 393, javax.swing.GroupLayout.PREFERRED_SIZE))
																				.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
																				.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
																						.addComponent(btnSetOutputDir, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)
																						.addComponent(btnSetTempDir, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)))
																						.addComponent(jLabel20, javax.swing.GroupLayout.PREFERRED_SIZE, 393, javax.swing.GroupLayout.PREFERRED_SIZE))
																						.addGap(0, 0, Short.MAX_VALUE)))
																						.addContainerGap())
				);
		jPanel3Layout.setVerticalGroup(
				jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(jPanel3Layout.createSequentialGroup()
						.addContainerGap()
						.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								.addComponent(txtOutputDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(jLabel8)
								.addComponent(btnSetOutputDir, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
										.addComponent(txtTempDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addComponent(jLabel9)
										.addComponent(btnSetTempDir, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
										.addComponent(jLabel16)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(jScrollPane5, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
										.addComponent(jLabel20)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 103, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addContainerGap(22, Short.MAX_VALUE))
				);

		jTabbedPane1.addTab("Misc", jPanel3);

		btnReplay.setBackground(new java.awt.Color(255, 255, 255));
		btnReplay.setIcon(new ImageIcon(loadIcon("/resources/icons/rewind.jpg")));
		btnReplay.setToolTipText("<html>\nStart in Replay-Mode: This mode replays a previously recorded sequence.<br>\nThe Rogue User will ask you for the sequence to replay.\n</html>");
		btnReplay.setFocusPainted(false);
		btnReplay.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnReplayActionPerformed(evt);
			}
		});

		btnView.setBackground(new java.awt.Color(255, 255, 255));
		btnView.setIcon(new ImageIcon(loadIcon("/resources/icons/view.jpg")));
		btnView.setToolTipText("<html>\nStart in View-Mode:<br>\nThe View-Mode allows you to inspect all steps of a previously recorded<br>\nsequence. Contrary to the Replay-Mode, it will not execute any actions,<br>\nbut only show you the screenshots that were recorded during sequence<br>\ngeneration. This is ideal if a sequence turns out not to be reproducible.\n</html>");
		btnView.setFocusPainted(false);
		btnView.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnViewActionPerformed(evt);
			}
		});

		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(layout.createSequentialGroup()
						.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
								.addComponent(jTabbedPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 505, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addGroup(layout.createSequentialGroup()
										.addComponent(btnSpy, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addGap(2, 2, 2)
										.addComponent(btnGenerate, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addGap(2, 2, 2)
										.addComponent(btnReplay, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
										.addComponent(btnView, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
										.addGap(0, 0, Short.MAX_VALUE)))
										.addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
				);
		layout.setVerticalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(layout.createSequentialGroup()
						.addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
						.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
								.addComponent(btnGenerate, javax.swing.GroupLayout.PREFERRED_SIZE, 129, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(btnSpy, javax.swing.GroupLayout.PREFERRED_SIZE, 129, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(btnReplay, javax.swing.GroupLayout.PREFERRED_SIZE, 129, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(btnView, javax.swing.GroupLayout.PREFERRED_SIZE, 129, javax.swing.GroupLayout.PREFERRED_SIZE))
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addComponent(jTabbedPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 372, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addContainerGap())
				);

		pack();
		Dimension scrDim = Toolkit.getDefaultToolkit().getScreenSize();
		setBounds((int) scrDim.getWidth() / 2 - getWidth() / 2, (int) scrDim.getHeight() / 3 - getHeight() / 2, getWidth(), getHeight());
	}// </editor-fold>//GEN-END:initComponents

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
	}//GEN-LAST:event_jButton1ActionPerformed

	private void btnGenerateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGenerateActionPerformed
		start(AbstractProtocol.Modes.Generate);
	}//GEN-LAST:event_btnGenerateActionPerformed

	private void btnSetOutputDirActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSetOutputDirActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fd.setCurrentDirectory(new File(settings.get(ConfigTags.OutputDir)).getParentFile());
		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			txtOutputDir.setText(file);
		}
	}//GEN-LAST:event_btnSetOutputDirActionPerformed

	private void btnSetTempDirActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSetTempDirActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fd.setCurrentDirectory(new File(settings.get(ConfigTags.TempDir)).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			txtTempDir.setText(file);
		}
	}//GEN-LAST:event_btnSetTempDirActionPerformed

	private void btnSpyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSpyActionPerformed
		start(AbstractProtocol.Modes.Spy);
	}//GEN-LAST:event_btnSpyActionPerformed

	private void btnReplayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReplayActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fd.setCurrentDirectory(new File(settings.get(ConfigTags.PathToReplaySequence)).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			settings.set(ConfigTags.PathToReplaySequence, file);
			start(AbstractProtocol.Modes.Replay);
		}
	}//GEN-LAST:event_btnReplayActionPerformed

	private void btnViewActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnViewActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fd.setCurrentDirectory(new File(settings.get(ConfigTags.PathToReplaySequence)).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			settings.set(ConfigTags.PathToReplaySequence, file);
			start(AbstractProtocol.Modes.View);
		}
	}//GEN-LAST:event_btnViewActionPerformed

	private void btnSaveSettingsAsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSaveSettingsAsActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fd.setCurrentDirectory(new File(settingsFile).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			extractInformation(settings);
			try {
				Util.saveToFile(settings.toFileString(), file);
				settingsFile = file;
			} catch (IOException e1) {
				Main.logln("Unable to save file: " + e1.toString());
			}

		}
	}//GEN-LAST:event_btnSaveSettingsAsActionPerformed

	private void btnEditProtocolActionPerformed(java.awt.event.ActionEvent evt) {
        JDialog dialog = new ProtocolEditor();
        dialog.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
        //dialog.setModalExclusionType(JDialog.ModalExclusionType.NO_EXCLUDE);
        dialog.setVisible(true);
	}
	
	private void btnLoadSettingsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoadSettingsActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fd.setCurrentDirectory(new File(settingsFile).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			try {
				settings = Main.loadSettings(new String[0], file);
				settingsFile = file;
				populateInformation(settings);
			} catch (ConfigException cfe) {
				Main.logln("Unable to load settings file!");
			}
		}
	}//GEN-LAST:event_btnLoadSettingsActionPerformed

	private void btnSutPathActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSutPathActionPerformed
		JFileChooser fd = new JFileChooser();
		fd.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fd.setCurrentDirectory(new File(settings.get(ConfigTags.Executable)).getParentFile());

		if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			String file = fd.getSelectedFile().getAbsolutePath();
			txtSutPath.setText(file);
		}
	}//GEN-LAST:event_btnSutPathActionPerformed

	private void tblCopyFromToMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_tblCopyFromToMouseClicked
		if (tblCopyFromTo.getSelectedColumn() >= 0 && tblCopyFromTo.getSelectedRow() >= 0) {
			JFileChooser fd = new JFileChooser();
			fd.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

			if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
				String file = fd.getSelectedFile().getAbsolutePath();
				tblCopyFromTo.setValueAt(file, tblCopyFromTo.getSelectedRow(), tblCopyFromTo.getSelectedColumn());
			} else {
				tblCopyFromTo.setValueAt(null, tblCopyFromTo.getSelectedRow(), tblCopyFromTo.getSelectedColumn());
			}
		}
	}//GEN-LAST:event_tblCopyFromToMouseClicked

	private void tblDeleteMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_tblDeleteMouseClicked
		if (tblDelete.getSelectedRow() >= 0 && tblDelete.getSelectedColumn() >= 0) {
			JFileChooser fd = new JFileChooser();
			fd.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

			if (fd.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
				String file = fd.getSelectedFile().getAbsolutePath();
				tblDelete.setValueAt(file, tblDelete.getSelectedRow(), tblDelete.getSelectedColumn());
			} else {
				tblDelete.setValueAt(null, tblDelete.getSelectedRow(), tblDelete.getSelectedColumn());
			}
		}
	}//GEN-LAST:event_tblDeleteMouseClicked

	private javax.swing.JButton btnGenerate;
	private javax.swing.JButton btnEditProtocol;
	private javax.swing.JButton btnLoadSettings;
	private javax.swing.JButton btnReplay;
	private javax.swing.JButton btnSaveSettingsAs;
	private javax.swing.JButton btnSetOutputDir;
	private javax.swing.JButton btnSetTempDir;
	private javax.swing.JButton btnSpy;
	private javax.swing.JButton btnSutPath;
	private javax.swing.JButton btnView;
	private javax.swing.JCheckBox checkForceForeground;
	private javax.swing.JCheckBox checkStopOnFault;
	private javax.swing.JCheckBox checkUseRecordedTimes;
	private javax.swing.JButton jButton1;
	private javax.swing.JLabel jLabel1;
	private javax.swing.JPanel aboutPanel;
	private javax.swing.JLabel jLabel10;
	private javax.swing.JLabel jLabel11;
	private javax.swing.JLabel jLabel12;
	private javax.swing.JLabel jLabel13;
	private javax.swing.JLabel jLabel14;
	private javax.swing.JLabel jLabel15;
	private javax.swing.JLabel jLabel16;
	private javax.swing.JLabel jLabel17;
	private javax.swing.JLabel jLabel18;
	private javax.swing.JLabel jLabel19;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JLabel jLabel20;
	private javax.swing.JLabel jLabel21;
	private javax.swing.JLabel jLabel22;
	private javax.swing.JLabel jLabel26;
	private javax.swing.JLabel jLabel27;
	private javax.swing.JLabel jLabel28;
	private javax.swing.JLabel jLabel23;
	private javax.swing.JLabel jLabel24;
	private javax.swing.JLabel jLabel25;
	private javax.swing.JLabel jLabel3;
	private javax.swing.JLabel jLabel4;
	private javax.swing.JLabel jLabel5;
	private javax.swing.JLabel jLabel6;
	private javax.swing.JLabel jLabel7;
	private javax.swing.JLabel jLabel8;
	private javax.swing.JLabel jLabel9;
	private javax.swing.JPanel jPanel1;
	private javax.swing.JPanel jPanel2;
	private javax.swing.JPanel jPanel3;
	private javax.swing.JPanel jPanel4;	
	private javax.swing.JPanel jPanel5;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JScrollPane jScrollPane2;
	private javax.swing.JScrollPane jScrollPane3;
	private javax.swing.JScrollPane jScrollPane4;
	private javax.swing.JScrollPane jScrollPane5;
	private javax.swing.JTabbedPane jTabbedPane1;
	private javax.swing.JSpinner spnActionDuration;
	private javax.swing.JSpinner spnActionWaitTime;
	private javax.swing.JSpinner spnFreezeTime;
	private javax.swing.JSpinner spnMaxTime;
	private javax.swing.JSpinner spnNumSequences;
	private javax.swing.JSpinner spnSequenceLength;
	private javax.swing.JSpinner spnSutStartupTime;
	private javax.swing.JSpinner spnVerbosity;
	private javax.swing.JTable tblCopyFromTo;
	private javax.swing.JTable tblDelete;
	private javax.swing.JTextArea txtClickFilter;
	private javax.swing.JTextField txtOutputDir;
	private javax.swing.JTextArea txtProcessFilter;
	private javax.swing.JTextArea txtSuspTitles;
	private javax.swing.JTextField txtSutPath;
	private javax.swing.JTextField txtTempDir;
}
