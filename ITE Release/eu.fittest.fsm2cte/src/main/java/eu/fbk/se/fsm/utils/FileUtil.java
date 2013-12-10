package eu.fbk.se.fsm.utils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.Comparator;

public class FileUtil {
	
	public static String[] listCTEfile(String directory){
		File dir = new File(directory);
		if (!dir.exists() || !dir.isDirectory())
			return null;
		
		String[] retList= dir.list(new FilenameFilter() {
			
			@Override
			public boolean accept(File f, String name) {
				if (name.endsWith(".cte"))
					return true;
				return false;
			}
		});
		
		Arrays.sort(retList, new Comparator<String>() {
			@Override
			public int compare(String cte1, String cte2) {
				
				if (cte1.matches("p[0-9]+\\.cte") && cte2.matches("p[0-9]+\\.cte")){
					int i1 = Integer.valueOf(cte1.substring(1, cte1.length() - 4)).intValue();
					int i2 = Integer.valueOf(cte2.substring(1, cte2.length() - 4)).intValue();
					
					if (i1 > i2) 
						return 1;
					else 
						return -1;
				}
				
				return 0;
			}
			
		});
		
		return retList;
	}
	
	
	/**
	 * 
	 * @param fin
	 * @param fout
	 * @throws Exception
	 */
	public static void copyFolder(File fin, File fout) throws Exception {
		String[] children = fin.list();
		if (children == null) {
			// Either dir does not exist or is not a directory
		} else {
			for (int p = 0; p < children.length; p++) {
				File f = new File(fin + File.separator + children[p]);
				File f1 = new File(fout + File.separator + children[p]);
				if (f.isDirectory())
					copyFolder(f, f1);
				else
					copyFile(f, f1);
			}
		}
	}
	 
	/**
	 * 
	 * @param inputFile
	 * @param outputFile
	 */
	 public static void copyFile(File inputFile, File outputFile) {
		int bufferSize = 4 * 1024;
		try {
			FileInputStream in = new FileInputStream(inputFile);
			FileOutputStream out = new FileOutputStream(outputFile);
			int readSize;
			byte buff[] = new byte[bufferSize];
			while ((readSize = in.read(buff)) != -1)
				out.write(buff, 0, readSize);
			in.close();
			out.close();
		} catch (Exception e) {
			System.out.println("Problem copying file: " + inputFile.getName());
//			e.printStackTrace();
		}
	}

	
}
