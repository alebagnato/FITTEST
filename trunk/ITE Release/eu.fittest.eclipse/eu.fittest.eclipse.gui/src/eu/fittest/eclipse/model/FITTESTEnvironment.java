package eu.fittest.eclipse.model;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class FITTESTEnvironment extends FITTESTArtifact {

	protected FITTESTEnvironment(FITTESTProject parent) {
		super(parent);
	}


	@Override
	public Object[] getChildren(Object parentElement) {
		if(_children==null){
			_children = new ArrayList<FITTESTArtifact>();
			_children.add(new FITTESTProduction(this));
			_children.add(new FITTESTTest(this));		
		}
		return _getChildren(parentElement);
	}

	@Override
	public String getText(Object element) {
		return "Environment";
	}

	@Override
	public Image getImage(Object element) {
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER);
	}
}
