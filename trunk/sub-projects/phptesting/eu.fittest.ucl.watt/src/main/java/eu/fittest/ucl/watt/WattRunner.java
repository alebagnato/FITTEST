/*****************************************************************************************************************************************************************************************************************/
Copyright (c) 2010-2050, UCL. All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
/*****************************************************************************************************************************************************************************************************************/

package eu.fittest.ucl.watt;

import java.io.File;
import java.io.IOException;

import eu.fittest.ucl.watt.utils.Utils;

public class WattRunner {
	public static int TIMEOUT = 60;
	private final Database dbUtils;
	private final File appDir;
	
	public WattRunner(Database db, File appDir) {
		this.appDir = appDir;
		this.dbUtils = db;
	}
	
	public boolean runWatt(String url, String applicationUsername, String applicationPassword) {
		String crawlerScript = null;
		String wattAppDir = null;
		boolean success = true;
		try {
			crawlerScript = new File(this.appDir, "crawler.pl").getCanonicalPath();
			wattAppDir = this.appDir.getCanonicalPath();
		} catch (IOException e) {
			Utils.log(e.getMessage());
			success = false;
		}
		if(success) {
			if(applicationUsername == null || applicationUsername.isEmpty())
				applicationUsername = "dummy";
			if(applicationPassword == null || applicationPassword.isEmpty())
				applicationPassword = "dummy";
			
			String[] args = new String[]{"perl",
					"-I", wattAppDir,
					crawlerScript,
					"-dbUser=" + this.dbUtils.getDbUser(),
					"-dbPassword=" + this.dbUtils.getDbPassword(),
					"-wattApp=" + wattAppDir, "-app=" + Database.getDBName(),
					"-login", "-user=" + applicationUsername, "-password=" + applicationPassword, 
					"-url=" + url, "-time=" + String.valueOf(TIMEOUT)};
			try {
				Process p = Runtime.getRuntime().exec(args);
				int status = p.waitFor();
				if(status != 0)
					success = false;
			} catch (IOException e) {
				Utils.log(e.getMessage());
				success = false;
			} catch (InterruptedException e) {
				Utils.log(e.getMessage());
				success = false;
			}
		}
		
		return success;
	}
}
