package eu.fittest.eclipse.model;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class FITTESTServers extends FITTESTArtifact{

	public FITTESTServers(FITTESTTest parent) {
		super(parent);
	}
	
	@Override
	public Object[] getChildren(Object parentElement) {
		if(_children==null){
			_children = new ArrayList<FITTESTArtifact>();
		}
		return _getChildren(parentElement);
	}
	@Override
	public String getText(Object element) {
		return "Servers";
	}

	@Override
	public Image getImage(Object element) {
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER);
	}
}
