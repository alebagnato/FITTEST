package eu.fittest.eclipse.component.junit.wizard;

import java.io.File;

public class SessionProperties {
	private File _tempFolder;
	private File _firefoxProfile;
	private Long _CRC;
	
	public SessionProperties() {
		_firefoxProfile = null;
		_tempFolder =null;
		_CRC = null;
	}
	
	public Long getCRC() {
		return _CRC;
	}

	public void setCRC(Long _CRC) {
		this._CRC = _CRC;
	}

	public File getTempFolder() {
		return _tempFolder;
	}
	public void setTempFolder(File tempFolder) {
		_tempFolder = tempFolder;
	}
	public File getFirefoxProfile() {
		return _firefoxProfile;
	}
	public void setFirefoxProfile(File firefoxProfile) {
		_firefoxProfile = firefoxProfile;
	}

}
