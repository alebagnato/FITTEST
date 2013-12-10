package eu.fittest.eclipse.gui.dot.test;

import java.io.File;

import net.claribole.zgrviewer.DOTManager;
import net.claribole.zgrviewer.ZGRViewer;

public class ZGRTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		 ZGRViewer view = new ZGRViewer(0);
		 view.getGraphicsManager().reset();
		 view.getGVLoader().loadFile(new File("all.dot"), DOTManager.DOT_PROGRAM, false);
		 //view.getGVLoader().openDOTFile(DOTManager.DOT_PROGRAM, false);
	}

}
