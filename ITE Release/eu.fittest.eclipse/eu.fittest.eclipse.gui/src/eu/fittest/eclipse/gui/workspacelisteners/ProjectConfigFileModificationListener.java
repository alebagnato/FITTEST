package eu.fittest.eclipse.gui.workspacelisteners;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.eclipse.gui.Activator;


public class ProjectConfigFileModificationListener implements IResourceChangeListener{

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		
		IResourceDelta resouceDelta = event.getDelta();
		if (resouceDelta != null){
			try {
				resouceDelta.accept(new IResourceDeltaVisitor() {
					@Override
					public boolean visit(IResourceDelta delta) throws CoreException {
						if (delta.getKind() == IResourceDelta.CHANGED)
						{
							int flags = delta.getFlags();
							if ((flags & IResourceDelta.CONTENT) != 0) {
								IResource resouce = delta.getResource();
								if (resouce.getName().endsWith("fittest.project")){
									IProject project = resouce.getProject();
									Activator.getDefault().updateProjectConfig(project);
								}
							}
						}
						return true;
					}
				});
			} catch (CoreException e) {
				e.printStackTrace();
			}
			
		}
	}

}
