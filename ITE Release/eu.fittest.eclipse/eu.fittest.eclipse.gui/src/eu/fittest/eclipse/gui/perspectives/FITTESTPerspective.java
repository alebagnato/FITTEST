package eu.fittest.eclipse.gui.perspectives;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class FITTESTPerspective implements IPerspectiveFactory {

	@Override
	public void createInitialLayout(IPageLayout layout) {
		//layout.addView("eu.fittest.eclipse.gui.tabs.TestArtifactsTab", IPageLayout.LEFT, IPageLayout.DEFAULT_FASTVIEW_RATIO,IPageLayout.ID_EDITOR_AREA);
	}

}
