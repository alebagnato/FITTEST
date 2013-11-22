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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import org.fruit.Util;
import jsyntaxpane.DefaultSyntaxKit;

public class ProtocolEditor extends javax.swing.JDialog {
    private static final long serialVersionUID = 5922037291232012481L;

    public ProtocolEditor() {
        DefaultSyntaxKit.initKit();
        initComponents();
        codeEditor.setContentType("text/java");
        codeEditor.setText(Util.readFile(new File("./CustomProtocol.java")));
    }

    private void initComponents() {
        jScrollPane1 = new javax.swing.JScrollPane();
        codeEditor = new javax.swing.JEditorPane();
        btnCompile = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        console = new javax.swing.JTextArea();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setMinimumSize(new java.awt.Dimension(800, 400));
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosed(java.awt.event.WindowEvent evt) {
                formWindowClosed(evt);
            }
        });

        codeEditor.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                codeEditorKeyPressed(evt);
            }
        });
        jScrollPane1.setViewportView(codeEditor);
        jScrollPane1.getVerticalScrollBar().setUnitIncrement(5);


        btnCompile.setText("Save and Compile");
        btnCompile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnCompileActionPerformed(evt);
            }
        });
        btnCompile.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                btnCompileKeyPressed(evt);
            }
        });

        console.setColumns(20);
        console.setLineWrap(true);
        console.setRows(5);
        console.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                consoleKeyPressed(evt);
            }
        });
        jScrollPane2.setViewportView(console);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1)
            .addComponent(jScrollPane2)
            .addComponent(btnCompile, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 570, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnCompile)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 122, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        pack();
    }                       


    private void compile() {
        try {
            console.setText("Compiling...");
            console.update(console.getGraphics());

            File compileDir = new File(".");
            Util.saveToFile(codeEditor.getText(), "./CustomProtocol.java");

            JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            if (compiler == null) {
                throw new RuntimeException("JDK required (running inside of JRE)");
            }

            DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<JavaFileObject>();
            StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnostics, null, null);
            try {
                Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromFiles(Util.getAllFiles(compileDir, ".java"));
                ArrayList<String> options = new ArrayList<String>();
                //options.add("-classpath \"" + System.getProperty("java.class.path") + "\\\"");
                options.add("-classpath");
                options.add(System.getProperty("java.class.path") + ";./monkey.jar");
//                for(String cp : System.getProperty("java.class.path").split(";")){
//                    if(new File(cp).isDirectory())
//                        cp = cp + "\\";
//                    options.add(cp);
//                }
                JavaCompiler.CompilationTask task = compiler.getTask(
                        null,
                        fileManager,
                        diagnostics,
                        options,
                        null,
                        compilationUnits);
                if (!task.call()) {
                    throw new RuntimeException("compile errors" + diagnostics.getDiagnostics().toString());
                }
            } finally {
                fileManager.close();
            }
            console.setText(console.getText() + "OK");
        } catch (Throwable t) {
            console.setText(console.getText() + "\n" + t.getMessage());
        }
    }


    private void btnCompileActionPerformed(java.awt.event.ActionEvent evt) {                                           
        compile();
    }                                          

    private void codeEditorKeyPressed(java.awt.event.KeyEvent evt) {                                      
        if ((evt.getModifiers() & ActionEvent.CTRL_MASK) == ActionEvent.CTRL_MASK) {
            if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
                compile();
            }
        }else if(evt.getKeyCode() == KeyEvent.VK_ESCAPE){
            this.dispose();
        }
    }                                     

    private void formWindowClosed(java.awt.event.WindowEvent evt) {                                  
        try {
            Util.saveToFile(codeEditor.getText(), "./CustomProtocol.java");
        } catch (IOException ioe) {
            System.out.println(ioe);
        }
    }                                 

    private void consoleKeyPressed(java.awt.event.KeyEvent evt) {                                   
        if(evt.getKeyCode() == KeyEvent.VK_ESCAPE)
            this.dispose();
    }                                  

    private void btnCompileKeyPressed(java.awt.event.KeyEvent evt) {                                      
        if(evt.getKeyCode() == KeyEvent.VK_ESCAPE)
            this.dispose();
    }                                     

    private javax.swing.JButton btnCompile;
    private javax.swing.JEditorPane codeEditor;
    private javax.swing.JTextArea console;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
}
