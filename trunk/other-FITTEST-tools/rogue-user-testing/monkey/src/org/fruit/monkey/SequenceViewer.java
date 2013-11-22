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
package org.fruit.monkey;

import static org.fruit.monkey.ConfigTags.PathToReplaySequence;
import java.awt.Canvas;
import java.awt.Graphics;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import org.fruit.Util;
import org.fruit.alayer.AWTCanvas;
import org.fruit.alayer.Action;
import org.fruit.alayer.Color;
import org.fruit.alayer.FillPattern;
import org.fruit.alayer.Image;
import org.fruit.alayer.Pen;
import org.fruit.alayer.State;
import org.fruit.alayer.Taggable;
import org.fruit.alayer.Rect;
import org.fruit.alayer.StdState;
import org.fruit.alayer.Tags;
import org.fruit.alayer.Visualizer;
import org.fruit.alayer.actions.NOP;


public class SequenceViewer extends java.awt.Frame {

	private static final long serialVersionUID = -7545369239319448135L;
	ObjectInputStream stream;
	BufferedImage buffer = new BufferedImage(1024, 768, BufferedImage.TYPE_INT_ARGB);
	int stateCount;

	public SequenceViewer(Settings settings) {
		this.settings = settings;
		initComponents();
		this.setBounds(0, 0, 1024, 768);
	}

	private void initComponents() {
		canvas1 = new java.awt.Canvas();
		panel1 = new java.awt.Panel();
		btnNext = new java.awt.Button();
		lblInfo = new java.awt.Label();
		panel2 = new java.awt.Panel();
		scrollPane1 = new java.awt.ScrollPane();

		display = new Canvas(){
			private static final long serialVersionUID = 5259423015295162447L;
			public void paint(Graphics g){
				double wfactor = (double)(display.getWidth()) / (double)buffer.getWidth();
				double hfactor = (double)(display.getHeight()) / (double)buffer.getHeight();
				double factor = Math.min(1.0, Math.min(wfactor, hfactor));					
				g.drawImage(buffer, 0, 0, (int)(buffer.getWidth() * factor), (int)(buffer.getHeight() * factor), null);
			}
		};


		addComponentListener(new ComponentAdapter(){
			public void componentResized(ComponentEvent e){
				display.setBounds(0, 0, buffer.getWidth(), buffer.getHeight());
			}
		});


		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				exitForm(evt);
			}
		});

		btnNext.setLabel("Next");
		btnNext.setName("");
		btnNext.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnNextActionPerformed(evt);
			}
		});

		lblInfo.setText("label1");

		javax.swing.GroupLayout panel1Layout = new javax.swing.GroupLayout(panel1);
		panel1.setLayout(panel1Layout);
		panel1Layout.setHorizontalGroup(
				panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(panel1Layout.createSequentialGroup()
						.addComponent(btnNext, javax.swing.GroupLayout.PREFERRED_SIZE, 73, javax.swing.GroupLayout.PREFERRED_SIZE)
						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
						.addComponent(lblInfo, javax.swing.GroupLayout.DEFAULT_SIZE, 617, Short.MAX_VALUE)
						.addContainerGap())
				);
		panel1Layout.setVerticalGroup(
				panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				.addGroup(panel1Layout.createSequentialGroup()
						.addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
								.addComponent(lblInfo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
								.addComponent(btnNext, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
								.addGap(0, 10, Short.MAX_VALUE))
				);

		add(panel1, java.awt.BorderLayout.NORTH);

		panel2.setLayout(new java.awt.BorderLayout());

		scrollPane1.add(display);

		panel2.add(scrollPane1, java.awt.BorderLayout.CENTER);

		add(panel2, java.awt.BorderLayout.CENTER);

		pack();
		setVisible(true);
		updateInfo("");
	}                      

	private void exitForm(java.awt.event.WindowEvent evt) {                          
		setVisible(false);
	}                         

	private void btnNextActionPerformed(java.awt.event.ActionEvent evt) {                                        
		try {
			nextPic();
			display.setBounds(0, 0, buffer.getWidth(), buffer.getHeight());
			display.repaint();
			display.paint(display.getGraphics());
			pack();
		} catch (IOException e1) {
			throw new RuntimeException(e1);
		} catch (ClassNotFoundException e1) {
			throw new RuntimeException(e1);
		}
	}                


	public void nextPic() throws IOException, ClassNotFoundException{
		if(stream == null){
			FileInputStream fis = new FileInputStream(new File(settings.get(PathToReplaySequence)));
			BufferedInputStream bis = new BufferedInputStream(fis);
			stream = new ObjectInputStream(bis);
		}

		Taggable fragment = null;

		try{
			fragment = (Taggable) stream.readObject();
		}catch(IOException ioe){
			return;
		}

		State state = fragment.get(Tags.SystemState, new StdState());

		Image img = state.get(Tags.Screenshot, null);
		if(img == null){
			AWTCanvas awtc = new AWTCanvas(0.0, 0.0, new BufferedImage(1024, 768, BufferedImage.TYPE_INT_ARGB), AWTCanvas.StorageFormat.PNG, 1.0);
			awtc.begin();
			Pen blackFill = Pen.newPen().setColor(Color.Black).setFillPattern(FillPattern.Solid).build();
			awtc.rect(blackFill, 0, 0, awtc.width(), awtc.height());
			Pen whiteText = Pen.newPen().setColor(Color.White).setFillPattern(FillPattern.Solid).setFontSize(100).build();
			awtc.text(whiteText, 0, awtc.height() / 2.0, 0, "No screenshot available!");
			awtc.end();
			img = awtc;
		}

		if(img.width() != buffer.getWidth() || img.height() != buffer.getHeight())
			buffer = new BufferedImage((int)img.width(), (int)img.height(), BufferedImage.TYPE_INT_ARGB);

		AWTCanvas cv = new AWTCanvas(0.0, 0.0, buffer, AWTCanvas.StorageFormat.PNG, 1);
		cv.begin();
		img.paint(cv, Rect.from(0, 0, img.width(), img.height()), Rect.from(0, 0, cv.width(), cv.height()));

		Action a = fragment.get(Tags.ExecutedAction, new NOP());
		Visualizer v = a.get(Tags.Visualizer, Util.NullVisualizer);
		v.run(state, cv, Pen.startFrom(Pen.DefaultPen).setColor(Color.Red).setFillPattern(FillPattern.Solid).build());
		cv.end();
		stateCount++;
		updateInfo(a.get(Tags.Desc, "<no description available>"));
	}
	
	public void updateInfo(String actionText){
		lblInfo.setText("State: " + Integer.toString(stateCount) + "  Action: " + actionText);
	}

	Settings settings;

	public void run() {
		while(isShowing())
			Util.pause(1);
	}

	private java.awt.Button btnNext;
	private java.awt.Canvas canvas1;
	private java.awt.Canvas display;
	private java.awt.Label lblInfo;
	private java.awt.Panel panel1;
	private java.awt.Panel panel2;
	private java.awt.ScrollPane scrollPane1;
}
