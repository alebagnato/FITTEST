package eu.fittest.component.appdescription.utils;

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.filechooser.FileFilter;

public class ExecutableJarFilter extends FileFilter {

	@Override
	public String getDescription() {
		return "Executable jar";
	}
	
	@Override
	public boolean accept(File f) {
		if(f.isDirectory()) return true;
		else if(f.getName().endsWith(".jar")){
			try {
				JarFile jarFile = new JarFile(f);
				Manifest mf = jarFile.getManifest();
				return (mf.getMainAttributes().getValue("Main-Class")!=null);
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.INFO,e.getMessage());
			}
			return false;
		}
		else return false;
	}

}
