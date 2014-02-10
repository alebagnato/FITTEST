package eu.fittest.eclipse.oraclesuite.actions;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.CoreException;

import eu.fittest.eclipse.daikon.utils.DaikonUtils;
import eu.fittest.eclipse.daikonorcsparser.utils.OrcsParserUtils;
import eu.fittest.eclipse.declmerge.utils.DeclMergeUtils;
import eu.fittest.eclipse.dtracesplit.utils.DTraceSplitUtils;
import eu.fittest.eclipse.haslog.utils.HaslogUtils;
import eu.fittest.eclipse.oraclesuite.utils.Utils;

public class PerlScripts {

//	private static String haslog ="D:\\Projects\\FITTEST\\fittest-svn\\Software\\SOFTEAM\\workspace\\eu.fittest.eclipse\\eu.fittest.eclipse.haslog\\resources\\bin\\haslogWin64";
//
//	private static String dtrsplit = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\SOFTEAM\\workspace\\eu.fittest.eclipse\\eu.fittest.eclipse.dtracesplit\\resources\\bin\\dtrsplitWin64";
//
//	private static String declmerge = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\SOFTEAM\\workspace\\eu.fittest.eclipse\\eu.fittest.eclipse.declmerge\\resources\\bin\\declmergeWin64";
//
//	private static String daikonjar = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\SOFTEAM\\workspace\\eu.fittest.eclipse\\eu.fittest.eclipse.daikon\\resources\\bin\\daikon.jar";
//		
//	private static String daikonice = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\SOFTEAM\\workspace\\eu.fittest.eclipse\\eu.fittest.eclipse.daikonorcsparser\\resources\\bin\\daikoniceWin64";	
//	
//	private static String java = "D:\\Java\\jdk1.6.0_45\\jre\\bin\\java";

		private static String haslog = HaslogUtils.getHasLogExecutable().getAbsolutePath();
	
		private static String dtrsplit = DTraceSplitUtils.getDtrsplitExecutable().getAbsolutePath() ;
	
		private static String declmerge = DeclMergeUtils.getDeclmergeExecutable().getAbsolutePath() ;
	
		private static String daikonjar = DaikonUtils.getDaikonJar().getAbsolutePath();
		
		private static String daikonice = OrcsParserUtils.getDaikonIceExecutable().getAbsolutePath();
	
		private static String java = "java"; 
		
		private static String confidenceLevel = "10";


	public static void prelogs(String logFolder){

		final String GHCRTopts = "+RTS -K200M -RTS" ;
		File logFolderFile = new File(logFolder);

		File[] existingListFile = logFolderFile.listFiles();
		List<File> existingFiles = Arrays.asList(existingListFile);

		boolean prelogNeeded = false;

		for(File existingFile : existingFiles){
			String name = existingFile.getName();
			if(name.endsWith(".log")){
				String nameWithoutExt = existingFile.getParentFile().getAbsolutePath() + File.pathSeparator + name.replaceAll(".log", "");
				File testLOX = new File(nameWithoutExt + ".lox");
				File testDIC = new File(nameWithoutExt + ".dic");
				File testXML = new File(nameWithoutExt + ".xml");
				if (!(testLOX.exists() && testDIC.exists() && testXML.exists()))
					prelogNeeded = true;
			}			
		}

		if (prelogNeeded){
			for(File existingFile : existingFiles){
				if(existingFile.getName().endsWith(".log")){
					//					String haslog = HaslogUtils.getHasLogExecutable().getAbsolutePath();

					String cmd = haslog + " " + GHCRTopts + " -c " + existingFile.getAbsolutePath();

					try {
						Process process = Runtime.getRuntime().exec(cmd, null, null);
						process.waitFor();
					} catch (IOException e) {
						e.printStackTrace(System.err);
					} catch (InterruptedException e) {
						e.printStackTrace(System.err);
					}


					cmd = haslog + " " + GHCRTopts + " -x --appEventOnly " + existingFile.getAbsolutePath();

					try {
						Process process = Runtime.getRuntime().exec(cmd, null, null);
						process.waitFor();
					} catch (IOException e) {
						e.printStackTrace(System.err);
					} catch (InterruptedException e) {
						e.printStackTrace(System.err);
					}


				}
			}
		}


	}

	public static void inferDaikon(String _logFolder, String _GHRCTopts, String _oracleFile, String _reportFile, String _eventsToInclude, List<String> _fieldsToInclude ) {

		File logFolder =  new File(_logFolder);

		prelogs(_logFolder);

		if (Utils.isPrepped(logFolder)){

			try{

				String daikonDir = logFolder.getAbsolutePath() + File.separator + "daikon";
				File daikonDirFile = new File(daikonDir);
				if (!daikonDirFile.exists())
					daikonDirFile.mkdirs();

				String eventFiltersOption = "" ;
				if (!_eventsToInclude.equals("")) {
					eventFiltersOption = "--appEvsSelect=" + _eventsToInclude ;
				}  

				// Let's do the STEP-2 above:
				// get the names of all source logs...
				List<String> logfiles = Utils.getFileNames(logFolder, ".log");
				// For every field that is to be included:
				for(String v : _fieldsToInclude) {
					String fieldDir = daikonDir + File.separator + "app." + v  + ".orc" ;
					// create subdir dtrace/field, if it does not exist:
					File fieldDirFile = new File(fieldDir);
					if(!(fieldDirFile.exists())) {
						fieldDirFile.mkdirs();
					}

					// For every source log-file:
					for( String f : logfiles) {
						String flog = logFolder.getAbsolutePath() + File.separator +  f ; 
						String daikonfile = fieldDir + File.separator + Utils.getBaseName(f) + ".dtrace" ;
						// convert to Daikon if this has not been done:

						String cmd = haslog + " "
								+ _GHRCTopts + " "
								+ "-d --output=" + daikonfile + " "
								+ eventFiltersOption + " "
								+ "--varsSelect=" + v + " "
								+ flog ;

						Process process = Runtime.getRuntime().exec(cmd) ;
						BufferedReader reader =
								new BufferedReader(new InputStreamReader(process.getInputStream()));
						while ((reader.readLine()) != null) {}
						process.waitFor();

					}         
				}

				/// New we do STEP 3 (inference):

				// For every field to include...
				for( String v : _fieldsToInclude) {
					String fieldDir  = daikonDir  + File.separator + "app." + v + ".orc" ;

					// get the names of all daikon logs in subdir app.v:
					List<String> daikonfiles = Utils.getFileNames(fieldDir,".dtrace") ;

					// Next, we construct the following list of paths:
					String daikonfiles_     = "" ;
					String daikonfiles2_    = "" ;
					String daikonDeclfiles_ = "" ;

					for( String f  : daikonfiles) {
						String fpath  = fieldDir + File.separator + f ;
						String  fpath2 = fieldDir + File.separator +  "separated" + File.separator + Utils.getBaseName(f) + ".recs.dtrace" ;
						String  fdecl = fieldDir  + File.separator + Utils.getBaseName(f) + ".decls" ;
						daikonfiles_  = daikonfiles_  + "  " + fpath  ;
						daikonfiles2_ = daikonfiles2_ + "  " + fpath2 ;
						daikonDeclfiles_ = daikonDeclfiles_ + "  " + fdecl ;
					}

					// Split and merge the Daikon logs:

					String cmd = dtrsplit + "  " + daikonfiles_ ;
					Process process = Runtime.getRuntime().exec(cmd) ;
					BufferedReader reader =
							new BufferedReader(new InputStreamReader(process.getInputStream()));
					while ((reader.readLine()) != null) {}
					process.waitFor();

					// Move the result into daikon/app.v.orc/separated:
					String separatedDir = fieldDir +  File.separator + "separated" ;
					File separatedDirFile = new File(separatedDir);
					if (!separatedDirFile.exists())
						separatedDirFile.mkdirs();

					cmd = declmerge + "  " + daikonDeclfiles_ ;
					process = Runtime.getRuntime().exec(cmd, null, separatedDirFile) ;
					reader =
							new BufferedReader(new InputStreamReader(process.getInputStream()));
					while ((reader.readLine()) != null) {}
					process.waitFor();


					List<File> dtraceFiles = Utils.collectFiles(fieldDir, ".recs.dtrace");
					for(File dtraceFile : dtraceFiles){
						Utils.moveFile(dtraceFile, new File(separatedDirFile.getAbsolutePath() + File.separator + dtraceFile.getName()));
					}


					List<File> declsFiles = Utils.collectFiles(new File(fieldDir), ".decls");
					for(File declsFile : declsFiles){
						declsFile.delete();
					}


					declsFiles = Utils.collectFiles(new File(fieldDir), ".decls");
					for(File declsFile : declsFiles){
						Utils.copyFile(declsFile, new File(separatedDir + File.separator + declsFile.getName()));
					}


					// Finally, infer the oracles:
					String invFilePath = fieldDir + File.separator + _oracleFile ;
					File invFile = new File(invFilePath);
					if (!invFile.exists()){
						invFile.createNewFile();
					}

					String mergedDeclPath = separatedDir  +  File.separator +  "merged.decls" ;
					File mergeDeclFile = new File(mergedDeclPath);
					if (!mergeDeclFile.exists()){
						mergeDeclFile.createNewFile();
					}

					String reportFilePath = fieldDir  +  File.separator + _reportFile ;
					File reportFile = new File(reportFilePath);
					if (!reportFile.exists()){
						reportFile.createNewFile();
					}

					cmd = java
							+ " -cp " + daikonjar + " "
							+ "daikon.Daikon -o " + invFilePath + " "
							+ "--nohierarchy --output_num_samples "
							+ mergedDeclPath + " " + daikonfiles2_ + " ";
					//							+ "> " + reportFile ;

					process = Runtime.getRuntime().exec(cmd) ;


					InputStream is = null;
					OutputStream os = null;
					try {
						is = process.getInputStream();
						os = new FileOutputStream(reportFile);
						byte[] buffer = new byte[1024];
						int length;
						while ((length = is.read(buffer)) > 0) {
							os.write(buffer, 0, length);
						}
					} finally {
						is.close();
						os.close();
					}

					process.waitFor();	

				}

			}catch(IOException e){
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}

	}

	public static void  inferllo(File logFolder,  String _GHRCTopts, String _oracleFile, String _reportFile, List<String> _functionsToInclude, String _otherLLOoptions ) {

		try {

			prelogs(logFolder.getAbsolutePath());

			if (Utils.isPrepped(logFolder)){

				File lloDir = new File(logFolder.getAbsolutePath() + File.separator  + "llo");
				if (!lloDir.exists())
					lloDir.mkdirs();

				List<String> logfiles =  Utils.getFileNames(logFolder, ".log");

				for(String f : logfiles){

					for(String function : _functionsToInclude){

						File flog = new File(logFolder.getAbsolutePath()  + File.separator + f);
						String daikonFile = lloDir.getAbsolutePath() + File.separator + "llo.dtrace"; 

						String cmd = haslog + " "
								+ _GHRCTopts + " "
								+ "-d "
								+ "--output=" + daikonFile + " "
								+ "--llo=" + function + " "
								+ _otherLLOoptions
								+ flog ;

						Process process = Runtime.getRuntime().exec(cmd) ;
						BufferedReader reader =
								new BufferedReader(new InputStreamReader(process.getInputStream()));
						while ((reader.readLine()) != null) {}
						process.waitFor();	

						List<String>  generatedDaikons = Utils.getFileNames(lloDir,".dtrace") ;

						for (String df : generatedDaikons){

							File functionSubdir =  new File( lloDir.getAbsolutePath() + File.separator +  df.subSequence(4, df.length() -7) + ".orc");

							if(!(functionSubdir.exists())) {
								functionSubdir.mkdirs();
							}

							String dfNewName = functionSubdir.getAbsolutePath()  + File.separator + Utils.getBaseName(f) + ".dtrace" ;
							Utils.moveFile(lloDir.getAbsolutePath() + File.separator + df, dfNewName) ;
						}
					}				

				}


				List<String> recognizedFunctions =  Utils.getFileNames(lloDir, ".orc");

				for (String fny : recognizedFunctions){

					String fnyPath = lloDir.getAbsolutePath() + File.separator + fny;

					List<String>  daikonfiles = Utils.getFileNames(new File(fnyPath), ".dtrace");

					String daikonfiles_     = "" ;
					String daikonfiles2_    = "" ;
					String daikonDeclfiles_ = "" ;

					for(String f : daikonfiles){
						String fpath  =  fnyPath + File.separator + f ;
						String fpath2 = fnyPath +  File.separator + "separated" + File.separator +Utils.getBaseName(f) + ".recs.dtrace" ;
						String fdecl = fnyPath +  File.separator + Utils.getBaseName(f) + ".decls" ;
						daikonfiles_  +=  "  "  + fpath  ;
						daikonfiles2_ +=  "  " + fpath2 ;
						daikonDeclfiles_  += "  " + fdecl ;
					}


					String cmd =  dtrsplit + "  "  + daikonfiles_;
					Process process = Runtime.getRuntime().exec(cmd) ;
					BufferedReader reader =
							new BufferedReader(new InputStreamReader(process.getInputStream()));
					while ((reader.readLine()) != null) {}
					process.waitFor();	

					String separatedDir = fnyPath +  File.separator + "separated" ;
					File separateDirFile = new File(separatedDir);
					if (!separateDirFile.exists()){
						separateDirFile.mkdirs();
					}


					cmd =  declmerge + "  "  + daikonDeclfiles_;
					process = Runtime.getRuntime().exec(cmd, null, separateDirFile) ;
					reader =
							new BufferedReader(new InputStreamReader(process.getInputStream()));
					while ((reader.readLine()) != null) {}
					process.waitFor();	

					// Move the result into daikon/app.v.orc/separated:


					List<File> dtraceFiles = Utils.collectFiles(new File(fnyPath), ".recs.dtrace");
					for(File dtraceFile : dtraceFiles){
						Utils.moveFile(dtraceFile, new File(separatedDir + File.separator + dtraceFile.getName()));
					}

					List<File> declsFiles = Utils.collectFiles(new File(fnyPath), ".decls");
					for(File declsFile : declsFiles){
						declsFile.delete();
					}


					//Finally, infer the oracles:
					String invFilePath = fnyPath  +  File.separator + _oracleFile ;
					File invFile = new File(invFilePath);
					if (!invFile.exists()){
						invFile.createNewFile();
					}

					String mergedDecl = separatedDir +  File.separator + "merged.decls" ;
					File mergeDeclFile = new File(mergedDecl);
					if (!mergeDeclFile.exists()){
						mergeDeclFile.createNewFile();
					}

					String reportFilePath = fnyPath +  File.separator + _reportFile ;
					File reportFile = new File(reportFilePath);
					if (!reportFile.exists()){
						reportFile.createNewFile();
					}


					cmd = java + " -cp " + 
							daikonjar + " " 
							+ "daikon.Daikon -o "  
							+ invFilePath + " "
							+ "--nohierarchy --output_num_samples "
							+  mergedDecl + " " + daikonfiles2_ ;
					//							+ " > "   
					//							+ reportFilePath + "\'";


					process = Runtime.getRuntime().exec(cmd) ;


					InputStream is = null;
					OutputStream os = null;
					try {
						is = process.getInputStream();
						os = new FileOutputStream(reportFile);
						byte[] buffer = new byte[1024];
						int length;
						while ((length = is.read(buffer)) > 0) {
							os.write(buffer, 0, length);
						}
					} finally {
						is.close();
						os.close();
					}

					process.waitFor();	
				}
				
				//daikon ice
				
				String cmd = daikonice + " -r ";
				
				for (String fny : recognizedFunctions){

					cmd += lloDir.getAbsolutePath() + File.separator + fny + File.separator + _reportFile + " ";
				}
				
				File reportHTMLFile = new File(logFolder.getAbsolutePath() + File.separator + "oracle.html ");
				if (!reportHTMLFile.exists())
					reportHTMLFile.createNewFile(); 
				
				cmd +=  " -n " + confidenceLevel + " -o " + reportHTMLFile.getAbsolutePath(); 
				Process process = Runtime.getRuntime().exec(cmd) ;

				process.waitFor();	
				System.err.println(process.waitFor());
			}


		} catch (CoreException e) {			
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	public static void checkDaikon(String _oldlogFolder, String _newlogFolder, String  _oracleFileName, String _violationFileName, String _reportFileName  ){

		File oraclesDir = new File(_oldlogFolder + File.separator + "daikon");
		if (!oraclesDir.exists()){
			oraclesDir.mkdirs();
		}

		File targetDaikonDir = new File(_newlogFolder + File.separator + "daikon");
		if (!targetDaikonDir.exists()){
			oraclesDir.mkdirs();
		}

		try {

			List<File> oraclesGroups =  Utils.collectFiles(oraclesDir, ".orc", "");

			for (File v : oraclesGroups){

				File oldGroupDir = new File(oraclesDir + File.separator + v.getName());
				File newGroupDir = new File(targetDaikonDir + File.separator + v.getName());
				File checkDir = new File(targetDaikonDir + File.separator + v.getName() + File.separator + "check");
				if (!checkDir.exists()){
					checkDir.mkdirs();
				}

				List<File> dtraceFiles =  Utils.collectFiles(newGroupDir, ".dtrace", "");
				String dtraces_ = "";

				for (File dtraceFile : dtraceFiles){
					dtraces_ += "  "  +  newGroupDir + File.separator + dtraceFile.getAbsolutePath() ;
				}

				File oracleFile  = new File(oldGroupDir.getAbsolutePath() + File.separator + _oracleFileName); 
			
				if (!oracleFile.exists()){
					oracleFile.createNewFile();
				}
				
				File violationReportFile = new File (checkDir.getAbsolutePath() + File.separator + _violationFileName);
				if (!violationReportFile.exists()){
					violationReportFile.createNewFile();
				}
				
				File reportDumpFile = new File (checkDir.getAbsolutePath()  + File.separator + _reportFileName);
				if (!reportDumpFile.exists()){
					reportDumpFile.createNewFile();
				}

				String cmd =  java + " -cp " + daikonjar  + " "
						+ "daikon.tools.InvariantChecker "
						+ "--verbose --conf --filter "
						+ "--verbose --conf "
						+ "--output " + violationReportFile.getAbsolutePath() + " "
						+ oracleFile.getAbsolutePath() + " "			      
						+ dtraces_ + " ";
//						+ " > " + reportDumpFile.getAbsolutePath() ;

				Process process = Runtime.getRuntime().exec(cmd) ;
				InputStream is = null;
				OutputStream os = null;
				try {
					is = process.getInputStream();
					os = new FileOutputStream(reportDumpFile);
					byte[] buffer = new byte[1024];
					int length;
					while ((length = is.read(buffer)) > 0) {
						os.write(buffer, 0, length);
					}
				} finally {
					is.close();
					os.close();
				}
				process.waitFor();				
			}


		} catch (CoreException e) {			
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	public static void checkllo(String _oldlogFolder, String _newlogFolder, String  _oracleFileName, String _violationFileName, String _reportFileName){

		File oraclesDir = new File(_oldlogFolder + File.separator + "llo"); 
		if(!(oraclesDir.exists())){
			//error or need Inference
		}
		
		File targetDaikonDir = new File(_newlogFolder + File.separator + "llo"); 
		if(!(targetDaikonDir.exists())){
			//error or need Inference
		}

		try {
			List<File> oraclesGroup = Utils.collectFiles(oraclesDir, ".orc");

			for (File oracle : oraclesGroup){

				File oldGroupDir = new File(oraclesDir.getAbsolutePath() + File.separator + oracle.getName());
				File newGroupDir = new File(targetDaikonDir.getAbsolutePath() + File.separator + oracle.getName());

				File checkDir = new File(targetDaikonDir.getAbsolutePath() + File.separator + oracle.getName() + File.separator + "check");
				if(!(checkDir.exists())){
					checkDir.mkdirs();
				}


				List<File> dtraceFiles =  Utils.collectFiles(newGroupDir, ".dtrace");
				String dtraces_ = "";

				for (File dtraceFile : dtraceFiles){
					dtraces_ += " " + newGroupDir.getAbsolutePath() + File.separator + dtraceFile.getName(); 
				}

				File oracleFile = new File(oldGroupDir.getAbsolutePath() + File.separator + _oracleFileName);
				if (!oracleFile.exists()){
					oracleFile.createNewFile();
				}
				
				File violationReportFile = new File(checkDir.getAbsolutePath() + File.separator + _violationFileName);
				if (!violationReportFile.exists()){
					violationReportFile.createNewFile();
				}
				
				File reportDumpFile = new File(checkDir.getAbsolutePath() + File.separator + _reportFileName);
				if (!reportDumpFile.exists()){
					reportDumpFile.createNewFile();
				}

				String cmd =  java + " -cp " + daikonjar + " "
						+ "daikon.tools.InvariantChecker "
						+ "--verbose --conf "
						+ "--output " + violationReportFile.getAbsolutePath() + " "
						+ oracleFile.getAbsolutePath() + " "			      
						+ dtraces_ + " " ;
				//						+ " > " + reportDumpFile.getAbsolutePath() ;

				Process process = Runtime.getRuntime().exec(cmd) ;
				InputStream is = null;
				OutputStream os = null;
				try {
					is = process.getInputStream();
					os = new FileOutputStream(reportDumpFile);
					byte[] buffer = new byte[1024];
					int length;
					while ((length = is.read(buffer)) > 0) {
						os.write(buffer, 0, length);
					}
				} finally {
					is.close();
					os.close();
				}

				process.waitFor();	

			}
		} catch (CoreException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}


}
