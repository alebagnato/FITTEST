package eu.fittest.eclipse.model;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class FITTESTTest extends FITTESTArtifact {

	public FITTESTTest(FITTESTEnvironment parent) {
		super(parent);
	}

	@Override
	public String getText(Object element) {
		return "Test";
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if(_children==null){
			_children = new ArrayList<FITTESTArtifact>();
			_children.add(new FITTESTServers(this));
			_children.add(new FITTESTClients(this));		
		}
		return _getChildren(parentElement);
	}
	
	@Override
	public Image getImage(Object element) {
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER);
	}

}
