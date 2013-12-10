package eu.fittest.fbk.efsm2ct.tools.files;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FileFinder {

	private String extension;
	
	private FileFilter extFilter = new FileFilter() {

		@Override
		public boolean accept(File pathname) {
			return pathname.toString().endsWith(extension);
		}
	};
	
	private FileFilter idFilter = new FileFilter() {

		@Override
		public boolean accept(File pathname) {
			return true;
		}
	};


	private FileFilter filter = new FileFilter() {

		@Override
		public boolean accept(File pathname) {
			return pathname.toString().endsWith(extension);
		}
	};

	private FileFilter fileFilter;

	public FileFinder() {
		
		fileFilter = idFilter;
		
	};

	private FileFilter dirFilter = new FileFilter() {

		@Override
		public boolean accept(File f) {
			return f.isDirectory();
		}
	};

	public FileFinder(String ext) {
		fileFilter = extFilter;
		extension = ext;
	}

	public List<File> locate(File dir, final String fileName) {

		List<File> res = new ArrayList<File>();
		
		FilenameFilter ff = new FilenameFilter() {
			
			@Override
			public boolean accept(File dir, String name) {
				
				return  name.equals(fileName);
			}
		};

		File[] found = dir.listFiles(ff);

		res.addAll(Arrays.asList(found));

		File[] subdirs = dir.listFiles(dirFilter);

		for (File d : subdirs) {

			res.addAll(locate(d,fileName));

		}

		return res;

	}
	
	public List<File> findFilesRecursively(File dir) {

		List<File> res = new ArrayList<File>();

		File[] found = dir.listFiles(filter);

		res.addAll(Arrays.asList(found));

		File[] subdirs = dir.listFiles(dirFilter);

		for (File d : subdirs) {

			res.addAll(findFilesRecursively(d));

		}

		return res;

	}

}
