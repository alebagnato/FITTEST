package eu.fittest.agent.sut.services.userdialog.impl;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.GridBagLayout;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URI;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.fittest.common.util.ITELogger;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import eu.fittest.agent.sut.services.userdialog.spec.IUserDialogService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.service.AbstractService;

public class UserDialogServiceImpl extends AbstractService implements IUserDialogService{
	public UserDialogServiceImpl() {
		_handlers.add(new UserDialogMH(this));
	}
	
	
	public void printMessage(final String message, final int type){
		ITELogger.log(Level.INFO, message);
		FITTESTSingleton.getThreadPool().execute(new Runnable() {
			
			public void run() {	
				
				/**
				 * Hack to display clickable URL for a recording session
				 * 
				 */
				String urlPattern =  "\\b(https?|ftp|file)://[A-Za-z0-9+&@#/%?=~_|!:,.;]*[A-Za-z0-9+&@#/%=~_|]";
				Pattern patt = Pattern.compile(urlPattern);
	            Matcher matcher = patt.matcher(message);
	            if (matcher.find()){
	            	String url = matcher.group();
	            	JFrame frame = new JFrame("Message from ITE");
	                frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	                frame.setSize(400, 100);
	                frame.setLocationRelativeTo(null);
	                Container container = frame.getContentPane();
	                container.setLayout(new BoxLayout(container, BoxLayout.PAGE_AXIS));
	                container.add(new JLabel(" "));
	                container.add(new JLabel("A recording session is about to start."));
	                container.add(linkify(frame, "Please Click on this URL: " + url, url, "Flexstore URL"));
	                frame.setVisible(true);
	            } else {
				
					JFrame frame = new JFrame();
					frame.setVisible(true);
					frame.toFront();
					JOptionPane.showMessageDialog(frame,message, "Message from ITE", type);
					frame.dispose();
	            }
			}
		});

	}
	
	/**
	 * Make textlabel clickable
	 * 
	 * thanks to:
	 * http://stackoverflow.com/questions/527719/how-to-add-hyperlink-in-jlabel
	 * @param frame 
	 * 
	 * @param text
	 * @param URL
	 * @param toolTip
	 * @return
	 */
	public static JLabel linkify(final JFrame frame, final String text, String URL, String toolTip)
	{
	    URI temp = null;
	    try
	    {
	        temp = new URI(URL);
	    }
	    catch (Exception e)
	    {
	        e.printStackTrace();
	    }
	    final URI uri = temp;
	    final JLabel link = new JLabel();
	    link.setText("<HTML><FONT color=\"#000099\">"+text+"</FONT></HTML>");
	    if(!toolTip.equals(""))
	        link.setToolTipText(toolTip);
	    link.setCursor(new Cursor(Cursor.HAND_CURSOR));
	    link.addMouseListener(new MouseListener()
	    {
	        public void mouseExited(MouseEvent arg0)
	        {
	            link.setText("<HTML><FONT color=\"#000099\">"+text+"</FONT></HTML>");
	        }

	        public void mouseEntered(MouseEvent arg0)
	        {
	            link.setText("<HTML><FONT color=\"#000099\"><U>"+text+"</U></FONT></HTML>");
	        }

	        public void mouseClicked(MouseEvent arg0)
	        {
	            if (Desktop.isDesktopSupported())
	            {
	                try
	                {
	                    Desktop.getDesktop().browse(uri);
	                    frame.dispose();
	                }
	                catch (Exception e)
	                {
	                    e.printStackTrace();
	                }
	            }
	            else
	            {
	                JOptionPane pane = new JOptionPane("Could not open link, please open it manually in your desired browser!");
	                JDialog dialog = pane.createDialog(new JFrame(), "");
	                dialog.setVisible(true);
	            }
	        }

	        public void mousePressed(MouseEvent e)
	        {
	        }

	        public void mouseReleased(MouseEvent e)
	        {
	        }
	    });
	    return link;
	}

	
	public String getName() {
		return IUserDialogService.class.getName();
	}

}
