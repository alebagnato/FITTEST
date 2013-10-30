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

import java.util.List;
import org.fruit.Pair;
import org.fruit.alayer.Tag;

public final class ConfigTags {
	public static final Tag<String> ProcessesToKillDuringTest = Tag.from("ProcessesToKillDuringTest", String.class);
	public static final Tag<Boolean> ShowVisualSettingsDialogOnStartup = Tag.from("ShowVisualSettingsDialogOnStartup", Boolean.class);
	public static final Tag<Integer> LogLevel = Tag.from("LogLevel", Integer.class);
	public static final Tag<String> SuspiciousTitles = Tag.from("SuspiciousTitles", String.class);
	public static final Tag<String> ClickFilter = Tag.from("ClickFilter", String.class);
	public static final Tag<String> OutputDir = Tag.from("OutputDir", String.class);
	public static final Tag<String> TempDir = Tag.from("TempDir", String.class);
	public static final Tag<Boolean> OnlySaveFaultySequences = Tag.from("OnlySaveFaultySequences", Boolean.class);
	public static final Tag<Boolean> ForceForeground = Tag.from("ForceForeground", Boolean.class);
	public static final Tag<Double> ActionDuration = Tag.from("ActionDuration", Double.class);
	public static final Tag<Double> FaultThreshold = Tag.from("FaultThreshold", Double.class);
	public static final Tag<Double> TimeToWaitAfterAction = Tag.from("TimeToWaitAfterAction", Double.class);
	public static final Tag<Boolean> VisualizeActions = Tag.from("VisualizeActions", Boolean.class);
	public static final Tag<Boolean> VisualizeSelectedAction = Tag.from("VisualizeSelectedAction", Boolean.class);
	public static final Tag<Boolean> DrawWidgetUnderCursor = Tag.from("DrawWidgetUnderCursor", Boolean.class);
	public static final Tag<Boolean> DrawWidgetInfo = Tag.from("DrawWidgetInfo", Boolean.class);
	public static final Tag<Boolean> ExecuteActions = Tag.from("ExecuteActions", Boolean.class);
	public static final Tag<String> PathToReplaySequence = Tag.from("PathToReplaySequence", String.class);
	public static final Tag<AbstractProtocol.Modes> Mode = Tag.from("Mode", AbstractProtocol.Modes.class);
	public static final Tag<String> Executable = Tag.from("Executable", String.class);
	public static final Tag<Integer> SequenceLength = Tag.from("SequenceLength", Integer.class);
	public static final Tag<Integer> Sequences = Tag.from("Sequences", Integer.class);
	public static final Tag<Double> ReplayRetryTime = Tag.from("ReplayRetryTime", Double.class);
	public static final Tag<Double> MaxTime = Tag.from("MaxTime", Double.class);
	public static final Tag<Double> StartupTime = Tag.from("StartupTime", Double.class);
	@SuppressWarnings("unchecked")
	public static final Tag<List<String>> Delete = Tag.from("Delete", (Class<List<String>>)(Class<?>)List.class);
	@SuppressWarnings("unchecked")
	public static final Tag<List<Pair<String, String>>> CopyFromTo = Tag.from("CopyFromTo", (Class<List<Pair<String, String>>>)(Class<?>)List.class);
	@SuppressWarnings("unchecked")
	public static final Tag<List<String>> MyClassPath = Tag.from("MyClassPath", (Class<List<String>>)(Class<?>)List.class);
	public static final Tag<String> OracleClass = Tag.from("OracleClass", String.class);
	public static final Tag<String> ActionBuilderClass = Tag.from("ActionBuilderClass", String.class);
	public static final Tag<String> SystemBuilderClass = Tag.from("SystemBuilderClass", String.class);
	public static final Tag<String> StateBuilderClass = Tag.from("StateBuilderClass", String.class);
	public static final Tag<String> ActionSelectorClass = Tag.from("ActionSelectorClass", String.class);
	public static final Tag<String> CanvasBuilderClass = Tag.from("CanvasBuilderClass", String.class);
	public static final Tag<String> ProtocolClass = Tag.from("ProtocolClass", String.class);
	public static final Tag<Boolean> UseRecordedActionDurationAndWaitTimeDuringReplay = Tag.from("UseRecordedActionDurationAndWaitTimeDuringReplay", Boolean.class);
	public static final Tag<Boolean> StopGenerationOnFault = Tag.from("StopGenerationOnFault", Boolean.class);
	public static final Tag<Double> TimeToFreeze = Tag.from("TimeToFreeze", Double.class);
	public static final Tag<Boolean> ShowSettingsAfterTest = Tag.from("ShowSettingsAfterTest", Boolean.class);
}
