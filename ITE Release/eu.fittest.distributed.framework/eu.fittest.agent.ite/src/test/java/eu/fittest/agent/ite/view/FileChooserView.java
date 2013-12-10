package eu.fittest.agent.ite.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.filechooser.FileFilter;

public class FileChooserView extends JFileChooser implements ActionListener{
	static private FileChooserView _instance = null;
	
	public static FileChooserView getInstance(){
		if(_instance==null) _instance = new FileChooserView();
		return _instance;
	}
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private FileChooserView() {
		setFileFilter(new FileFilter() {
			
			@Override
			public String getDescription() {
				return ".swf";
			}
			
			@Override
			public boolean accept(File file) {
				return file.getName().endsWith(".swf") || file.isDirectory();
			}
		});
	}

	public void actionPerformed(ActionEvent e) {
		showOpenDialog((JMenuItem)e.getSource());
	}

}
