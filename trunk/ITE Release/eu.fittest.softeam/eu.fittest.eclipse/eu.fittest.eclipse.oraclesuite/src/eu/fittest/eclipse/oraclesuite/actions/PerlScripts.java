package eu.fittest.eclipse.oraclesuite.actions;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.CoreException;

import eu.fittest.eclipse.declmerge.utils.DeclMergeUtils;
import eu.fittest.eclipse.dtracesplit.utils.DTraceSplitUtils;
import eu.fittest.eclipse.haslog.utils.HaslogUtils;
import eu.fittest.eclipse.oraclesuite.utils.Daikon;
import eu.fittest.eclipse.oraclesuite.utils.Utils;

public class PerlScripts {


	public static void inferDaikon(String _logFolder, String _GHRCTopts, String _oracleFile, String _reportFile, String _eventsToInclude, List<String> _fieldsToInclude ) {
		String haslog = HaslogUtils.getHasLogExecutable().getAbsolutePath();

		String dtrsplit = DTraceSplitUtils.getDtrsplitExecutable().getAbsolutePath() ;

		String declmerge = DeclMergeUtils.getDeclmergeExecutable().getAbsolutePath() ;

		String daikonjar = Daikon.getDaikonJar().getAbsolutePath();

		File logFolder =  new File(_logFolder);

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

						Process process = Runtime.getRuntime().exec(cmd, null, null) ;
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
					Process process = Runtime.getRuntime().exec(cmd, null, null) ;
					process.waitFor();

					cmd = declmerge + "  " + daikonDeclfiles_ ;
					process = Runtime.getRuntime().exec(cmd, null, null) ;
					process.waitFor();


					// Move the result into daikon/app.v.orc/separated:
					String separatedDir = fieldDir +  File.separator + "separated" ;
					File separatedDirFile = new File(separatedDir);
					if (!separatedDirFile.exists())
						separatedDirFile.mkdirs();

					List<File> dtraceFiles = Utils.collectFiles(fieldDir, ".recs.dtrace");
					Utils.copyFiles(dtraceFiles, separatedDir);


					List<File> declsFiles = Utils.collectFiles(new File(fieldDir), ".decls");
					for(File declsFile : declsFiles){
						declsFile.delete();
					}


					declsFiles = Utils.collectFiles(new File(fieldDir), ".decls");
					for(File declsFile : declsFiles){
						Utils.copyFile(declsFile, new File(separatedDir + File.separator + declsFile.getName()));
					}


					// Finally, infer the oracles:
					String invFile    = fieldDir + File.separator + _oracleFile ;
					String mergedDecl = separatedDir  +  File.separator +  "merged.decls" ;
					String reportFile = fieldDir  +  File.separator + _reportFile ;
					cmd = "java "
							+ "-cp " + daikonjar + " "
							+ "daikon.Daikon -o " + invFile + " "
							+ "--nohierarchy --output_num_samples "
							+ mergedDecl + " " + daikonfiles2_ + " "
							+ "> " + reportFile ;

					process = Runtime.getRuntime().exec(cmd, null, null) ;
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

		String haslog = HaslogUtils.getHasLogExecutable().getAbsolutePath();

		String dtrsplit = DTraceSplitUtils.getDtrsplitExecutable().getAbsolutePath() ;

		String declmerge = DeclMergeUtils.getDeclmergeExecutable().getAbsolutePath() ;

		String daikonjar = Daikon.getDaikonJar().getAbsolutePath();


		try {

			if (Utils.isPrepped(logFolder)){

				File lloDir = new File(logFolder.getAbsolutePath() + File.separator  + "llo");
				if (!lloDir.exists())
					lloDir.mkdirs();

				List<String> logfiles =  Utils.getFileNames(logFolder, ".log");

				for(String f : logfiles){

					for(String function : _functionsToInclude){

						File flog = new File(logFolder.getAbsolutePath()  + File.separator + f);
						File daikonFile = new File(lloDir.getAbsolutePath() + File.separator + "llo.dtrace"); 

						String cmd = haslog + " "
								+ _GHRCTopts + " "
								+ "-d "
								+ "--output=" + daikonjar + " "
								+ "--llo=" + function + " "
								+ _otherLLOoptions
								+ flog ;

						Process process = Runtime.getRuntime().exec(cmd, null, logFolder) ;
						process.waitFor();	

						List<String>  generatedDaikons = Utils.getFileNames(lloDir,".dtrace") ;

						for (String df : generatedDaikons){


							File functionSubdir =  new File( lloDir.getAbsolutePath() + File.separator +  df.subSequence(4, df.length() -11) + ".orc");

							if(!(functionSubdir.exists())) {
								functionSubdir.mkdirs();
							}

							String dfNewName = functionSubdir.getAbsolutePath()  + File.separator + Utils.getBaseName(f) + ".dtrace" ;
							Utils.copyFile(lloDir.getAbsolutePath() + File.separator + df, dfNewName) ;
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
						daikonfiles_  = daikonfiles_  + "  "  + fpath  ;
						daikonfiles2_ = daikonfiles2_ + "  " + fpath2 ;
						daikonDeclfiles_ = daikonDeclfiles_ + "  " + fdecl ;
					}


					String cmd =  dtrsplit + "  "  + daikonfiles_;
					Process process = Runtime.getRuntime().exec(cmd, null, logFolder) ;
					process.waitFor();

					cmd =  declmerge + "  "  + daikonDeclfiles_;
					process = Runtime.getRuntime().exec(cmd, null,logFolder) ;
					process.waitFor();

					// Move the result into daikon/app.v.orc/separated:
					String separatedDir = fnyPath +  File.separator + "separated" ;
					File separateDirFile = new File(separatedDir);
					if (!separateDirFile.exists()){
						separateDirFile.mkdirs();
					}

					List<File> dtraceFiles = Utils.collectFiles(new File(fnyPath), ".recs.dtrace");
					for(File dtraceFile : dtraceFiles){
						Utils.copyFile(dtraceFile, new File(separatedDir + File.separator + dtraceFile.getName()));
					}


					List<File> declsFiles = Utils.collectFiles(new File(fnyPath), ".decls");
					for(File declsFile : declsFiles){
						declsFile.delete();
					}


					declsFiles = Utils.collectFiles(new File(fnyPath), ".decls");
					for(File declsFile : declsFiles){
						Utils.copyFile(declsFile, new File(separatedDir + File.separator + declsFile.getName()));
					}

					//Finally, infer the oracles:
					String invFile = fnyPath  +  File.separator + _oracleFile ;
					String mergedDecl = separatedDir +  File.separator + "merged.decls" ;
					String reportFile = fnyPath +  File.separator + _reportFile ;

					cmd =  "java "
							+ "-cp " + daikonjar + " "
							+ "daikon.Daikon -o "  + invFile + " "
							+ "--nohierarchy --output_num_samples "
							+ mergedDecl + " " + daikonfiles2_ + " "
							+ "> "  + reportFile;

					process = Runtime.getRuntime().exec(cmd, null, logFolder) ;
					process.waitFor();

				}

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

				List<File> dtraceFiles =  Utils.collectFiles(newGroupDir, ".dtrace", "");
				String dtraces_ = "";

				for (File dtraceFile : dtraceFiles){
					dtraces_ += "  "  +  newGroupDir + File.separator + dtraceFile.getAbsolutePath() ;
				}

				File oracleFile  = new File(oldGroupDir.getAbsolutePath() + File.separator + _oracleFileName); ;
				File violationReportFile = new File (checkDir + File.separator + _violationFileName);
				File reportDumpFile      = new File (checkDir  + File.separator + _reportFileName);

				String cmd =  "java  " + "-cp " + Daikon.getDaikonJar().getAbsolutePath() + " "
						+ "daikon.tools.InvariantChecker "
						+ "--verbose --conf --filter "
						+ "--verbose --conf "
						+ "--output " + violationReportFile.getAbsolutePath() + " "
						+ oracleFile.getAbsolutePath() + " "			      
						+ dtraces_ + " "
						+ " > " + reportDumpFile.getAbsolutePath() ;

				Process process = Runtime.getRuntime().exec(cmd, null, null) ;
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

}
