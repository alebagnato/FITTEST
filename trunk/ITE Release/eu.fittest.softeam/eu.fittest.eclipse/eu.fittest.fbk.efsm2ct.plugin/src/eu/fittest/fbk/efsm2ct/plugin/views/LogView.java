package eu.fittest.fbk.efsm2ct.plugin.views;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;

import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;

public class LogView extends ViewPart implements LogConsumer {

	private StringBuilder line = new StringBuilder();
	private Text text;
	private boolean active = true;
	private int lineCount;

	public LogView() {
	}

	public void createPartControl(Composite parent) {

		LogViewClearAction clearAction = new LogViewClearAction(this);

		getViewSite().getActionBars().getToolBarManager().add(clearAction);

		// GridLayout rowLayout = new GridLayout(1,false);
		// parent.setLayout(rowLayout);
		//
		// ToolBar bar = new ToolBar (parent, SWT.NONE);
		//
		// ToolItem item = new ToolItem (bar, SWT.NONE);
		// item.setText("Clear");

		// Rectangle clientArea = parent.getClientArea();
		// bar.setLocation(clientArea.x, clientArea.y);

		text = new Text(parent, SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);

	}

	@Override
	public void setFocus() {

		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				text.setFocus();
			}

		});

	}

	@Override
	public void consume(char ch) {

		if (active) {

			if (ch == '\n') { // TODO this depends on the OS

				String l = line.toString();

				if (ConfigurationFactory.getInstance().isLogView()
						&& (ConfigurationFactory.getInstance().isVerboseLogView() || 
								!l.startsWith("RT") || 
								(ConfigurationFactory.getInstance().isLogViewTrasitions() && l.contains("->")) )) {

					line.append(Text.DELIMITER);

					lineCount++;

					// if (false && lineCount > 2500) {
					// active = false;
					// scheduleTextUpdate("log suspended after:"+lineCount+" lines");
//				} else {
					scheduleTextUpdate(line.toString());
//				}

			}

			line.delete(0, line.length());

		} else { // ch != '\n'
			line.append(ch);
		}
			
		} // !active

	}

	public void scheduleTextUpdate(final String line) {

		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				text.append(line);
			}

		});
	}

	public void clear() {

		text.setText("");

	}

}