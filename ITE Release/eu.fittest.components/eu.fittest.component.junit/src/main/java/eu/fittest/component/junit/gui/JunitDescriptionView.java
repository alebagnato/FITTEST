package eu.fittest.component.junit.gui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.logging.Level;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;

import org.openqa.selenium.android.library.Logger;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.component.junit.IJunitDescriptionView;
import eu.fittest.component.junit.JUnitComponent;
import eu.fittest.component.junit.WebBrowser;

public class JunitDescriptionView extends JFrame implements IJunitDescriptionView{
	private static final long serialVersionUID = 1L;
	private JComboBox _browser;
	
	private JTextArea _text;
	// begin by urueda
	private int _textAdditions = 0;
	private static final int MAX_TEXT_ADDITIONS = 256;
	private static final int KEEP_TEXT_LINES = 256;
	// end by urueda
	
	private PropertyChangeSupport _support;
	
	
	public void addPropertyChangeListener(PropertyChangeListener l){
		_support.addPropertyChangeListener(l);
	}
	
	public void removePropertyChangeListener(PropertyChangeListener l){
		_support.removePropertyChangeListener(l);
	}
	
	public WebBrowser getSelectedWebBrowser(){
		return (WebBrowser) _browser.getSelectedItem();
	}
	
	public JunitDescriptionView() throws FITTESTException {
		super(JUnitComponent.class.getName());		
		setMinimumSize(new Dimension(380, 300));
		_support = new PropertyChangeSupport(this);
		
		getContentPane().setLayout(new GridBagLayout());
		
		///////////////////////////////1st LINE/////////////////////////////////////////////////////////////////////
		GridBagConstraints c = new GridBagConstraints(0,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
				GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		
		JLabel lbl = new JLabel("Select Web browser to run:");
		getContentPane().add(lbl,c);
		
		 c = new GridBagConstraints(1,0,1,1,1.0,1.0,GridBagConstraints.LINE_START,
					GridBagConstraints.HORIZONTAL,new Insets(1,1,1,1),0,0);
		 
		_browser = new JComboBox(WebBrowser.values());
		_browser.setEditable(false);
		_browser.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				_support.firePropertyChange("selenium.browserStartCommand", null, _browser.getSelectedItem());
			}
		});
		getContentPane().add(_browser,c);
		
		c = new GridBagConstraints(0,1,2,1,1.0,5.0,GridBagConstraints.LINE_START,
				GridBagConstraints.BOTH,new Insets(1,1,1,1),0,0);
		
		_text = new JTextArea();
		JScrollPane pane = new JScrollPane(_text);
		pane.setBorder(BorderFactory.createTitledBorder("Selenium logs"));
		_text.setEditable(false);
		getContentPane().add(pane,c);
		
		//pack();
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	}
	

	@Override
	public void close() {
		dispose();		
	}

	@Override
	public void open() {
		setVisible(true);
		_text.setText(null);
	}

	@Override
	public void appendText(String text) {
		_text.append(text+"\n");
		// begin by urueda: manage the ammount of text to avoid OutOfMemory in JAVA HEAP SPACE
		_textAdditions++;
		if (_textAdditions > MAX_TEXT_ADDITIONS) {
			_textAdditions = 0;
			cleanupText();
		}
		// end by urueda
	}

	// by urueda
	private void cleanupText() {
		Document doc = _text.getDocument();
		Element root = doc.getDefaultRootElement();
		while (root.getElementCount() > KEEP_TEXT_LINES) {
			Element firstLine = root.getElement(0);
			try {
				doc.remove(0, firstLine.getEndOffset());
			} catch(BadLocationException ble) {
				Logger.getLogger().log(Level.WARNING, "JUnit component log console: could not clean-up console - size = " + root.getElementCount());
			}
		}		
	}
	
}
