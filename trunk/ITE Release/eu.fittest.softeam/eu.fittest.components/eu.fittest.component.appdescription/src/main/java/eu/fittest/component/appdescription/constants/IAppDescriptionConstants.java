package eu.fittest.component.appdescription.constants;

import javax.swing.ImageIcon;

import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.component.appdescription.gui.AppDescriptionView;

public interface IAppDescriptionConstants {
	static final String CONF_FILE=FITTESTConstants.FITTEST_SETTINGS_DIR+"appdescriptioncomponent.conf";
	static final String URL="appdescriptioncomponent.url";
	static final String PORT="appdescriptioncomponent.port";
	static final String HOST="appdescriptioncomponent.host";
	static final String PATH="appdescriptioncomponent.webpath";
	static final String PROTOCOL="appdescriptioncomponent.protocol";
	static final String EXECUTABLE_JAR="appdescriptioncomponent.executablejar";
	static final String FOLDER = "appdescriptioncomponent.defaultfolder";
	static final ImageIcon OK = new ImageIcon(AppDescriptionView.class.getResource("/icons/ok.png"));
	static final ImageIcon NOT_OK = new ImageIcon(AppDescriptionView.class.getResource("/icons/notok.png"));
}
