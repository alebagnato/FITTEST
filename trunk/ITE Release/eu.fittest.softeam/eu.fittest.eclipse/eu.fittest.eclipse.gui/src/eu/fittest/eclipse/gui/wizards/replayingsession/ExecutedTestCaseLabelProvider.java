package eu.fittest.eclipse.gui.wizards.replayingsession;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

import eu.fittest.eclipse.model.jobs.replay.TestCaseExecution;

public class ExecutedTestCaseLabelProvider implements ILabelProvider{

	@Override
	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Image getImage(Object element) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getText(Object element) {
		if(element instanceof TestCaseExecution){
			TestCaseExecution tc = (TestCaseExecution) element;
			String label = tc.getTestCaseName()+" on "+tc.getSourceAgentId();
			if(tc.getVerdict()!=null){
				label+=": "+tc.getVerdict();
				if(tc.getMessage()!=null){
					label+=" ("+tc.getMessage()+")";
				}
			}
			else{
				label+=": "+tc.getStatus();
			}
			return label;
		}
		else return null;
	}

}
