/*****************************************************************************************************************************************************************************************************************/
Copyright (c) 2010-2050, UCL. All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
/*****************************************************************************************************************************************************************************************************************/


package eu.fittest.ucl.watt.utils;

import java.util.logging.Logger;

public class Utils {
	private static Logger logger = Logger.getLogger("eu.fittest.ucl.watt");
	
	public static void setLogger(Logger _logger) {
		logger = _logger;
	}
	
	public static void log(String msg) {
		if(logger != null)
			logger.info(msg);
	}
}
