/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.monkey;

import static org.fruit.monkey.ConfigTags.*;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.fruit.Pair;
import org.fruit.UnProc;
import org.fruit.Util;

public class Main {
	public static enum LogLevel{ 
		Critical(0), Info(1), Debug(2);
		final int significance;
		LogLevel(int significance){ this.significance = significance; }
		public int significance(){ return significance; }
	}

	private static int logLevel = 0;
	private static PrintStream log;

	public static void main(String[] args) throws IOException{
		Settings settings = null;
		try{
			settings = loadSettings(args, null);

			if(settings.get(ConfigTags.ShowVisualSettingsDialogOnStartup)){
				if((settings = new SettingsDialog().run(settings, "./test.settings")) == null)
					return;
			}

			try{
				logLevel = settings.get(LogLevel);
				String logFileName = Util.dateString("yyyy_MM_dd__HH_mm_ss") + ".log";
				File logFile = new File(settings.get(OutputDir) + File.separator + logFileName);
				if(logFile.exists())
					logFile = Util.generateUniqueFile(settings.get(OutputDir), logFileName);
				log = new PrintStream(new BufferedOutputStream(new FileOutputStream(logFile)));
			}catch(Throwable t){
				System.out.println("Cannot initialize log file!");
				t.printStackTrace(System.out);
				System.exit(-1);
			}

			logln(Util.dateString("dd.MMMMM.yyyy HH:mm:ss"));
			logln("Hello, I'm the FRUIT Monkey!" + Util.lineSep() + Util.lineSep() + "These are my settings:", Main.LogLevel.Critical);
			logln(settings.toString(), Main.LogLevel.Critical);
			List<String> cp = settings.get(MyClassPath);
			URL[] classPath = new URL[cp.size()];
			for(int i = 0; i < cp.size(); i++)
				classPath[i] = new File(cp.get(i)).toURI().toURL();
			URLClassLoader loader = new URLClassLoader(classPath);

			logln("Trying to load monkey protocol in class '" + settings.get(ProtocolClass) + "' with class path '" + Util.toString(cp) + "'", Main.LogLevel.Debug);
			@SuppressWarnings("unchecked")
			UnProc<Settings> protocol = (UnProc<Settings>)loader.loadClass(settings.get(ProtocolClass)).getConstructor().newInstance();
			logln("Monkey protocol loaded!", Main.LogLevel.Debug);

			logln("Starting monkey protocol ...", Main.LogLevel.Debug);
			protocol.run(settings);
		}catch (ConfigException ce){
			logln("There is an issue with the configuration file: " + ce.getMessage(), Main.LogLevel.Critical);
		} catch (Throwable t) {
			logln("An unexpected error occurred: " + t, Main.LogLevel.Critical);
			t.printStackTrace(System.out);
			t.printStackTrace(log);
		}finally{
			logln("Monkey stopped execution.", Main.LogLevel.Critical);
			logln(Util.dateString("dd.MMMMM.yyyy HH:mm:ss"));
			if(log != null)
				log.flush();
			//if(settings != null && settings.get(ShowSettingsAfterTest))
				//Runtime.getRuntime().exec("cmd /c start monkey.bat && exit");
			System.exit(0);
		}
	}

	public static Settings loadSettings(String[] argv, String file) throws ConfigException{
		try{
			List<Pair<?, ?>> defaults = new ArrayList<Pair<?, ?>>();

			defaults.add(Pair.from(ProcessesToKillDuringTest, "(?!x)x"));
			defaults.add(Pair.from(ShowVisualSettingsDialogOnStartup, true));
			defaults.add(Pair.from(FaultThreshold, 0.1));
			defaults.add(Pair.from(LogLevel, 1));
			defaults.add(Pair.from(Mode, AbstractProtocol.Modes.Spy));		
			defaults.add(Pair.from(OutputDir, "."));
			defaults.add(Pair.from(TempDir, "."));		
			defaults.add(Pair.from(OnlySaveFaultySequences, false));
			defaults.add(Pair.from(PathToReplaySequence, "./temp/tmpsequence"));
			defaults.add(Pair.from(ActionDuration, 0.1));
			defaults.add(Pair.from(TimeToWaitAfterAction, 0.1));
			defaults.add(Pair.from(ExecuteActions, true));
			defaults.add(Pair.from(DrawWidgetUnderCursor, false));
			defaults.add(Pair.from(DrawWidgetInfo, true));
			defaults.add(Pair.from(VisualizeActions, false));
			defaults.add(Pair.from(VisualizeSelectedAction, false));
			defaults.add(Pair.from(SequenceLength, 10));
			defaults.add(Pair.from(ReplayRetryTime, 30.0));
			defaults.add(Pair.from(Sequences, 1));
			defaults.add(Pair.from(MaxTime, 31536000.0));
			defaults.add(Pair.from(StartupTime, 8.0));
			defaults.add(Pair.from(Executable, "C:/Windows/System32/calc.exe"));		
			defaults.add(Pair.from(Delete, new ArrayList<String>()));
			defaults.add(Pair.from(CopyFromTo, new ArrayList<Pair<String, String>>()));
			defaults.add(Pair.from(SuspiciousTitles, "(?!x)x"));
			defaults.add(Pair.from(ClickFilter, "(?!x)x"));
			defaults.add(Pair.from(MyClassPath, Arrays.asList(".")));
			defaults.add(Pair.from(ProtocolClass, "org.fruit.monkey.DefaultProtocol"));
			defaults.add(Pair.from(ForceForeground, true));
			defaults.add(Pair.from(UseRecordedActionDurationAndWaitTimeDuringReplay, true));
			defaults.add(Pair.from(StopGenerationOnFault , true));
			defaults.add(Pair.from(TimeToFreeze, 10.0));
			defaults.add(Pair.from(ShowSettingsAfterTest, true));

			return Settings.fromFile(defaults, file == null ? "./test.settings" : file);
		}catch(IOException ioe){
			throw new ConfigException("Unable to load configuration file!", ioe);
		}
	}

	public static void logln(String string, LogLevel level){
		if(level.significance() <= logLevel){
			System.out.println(string);
			if(log != null)
				log.println(string);
			System.out.flush();
		}
	}

	public static void logln(String string){ logln(string, Main.LogLevel.Info); }

	public static void log(String string, LogLevel level){
		if(level.significance() <= logLevel){
			System.out.print(string);
			if(log != null)
				log.print(string);
			System.out.flush();
		}
	}

	public static void log(String string){ log(string, Main.LogLevel.Info); }
}
