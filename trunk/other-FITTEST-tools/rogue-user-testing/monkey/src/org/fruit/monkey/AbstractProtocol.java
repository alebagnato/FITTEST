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

import static org.fruit.alayer.Tags.*;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.RandomAccessFile;
import java.util.EnumSet;
import java.util.Set;
import org.fruit.UnProc;
import org.fruit.Util;
import org.fruit.alayer.ActionBuildException;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Color;
import org.fruit.alayer.FillPattern;
import org.fruit.alayer.Action;
import org.fruit.alayer.Canvas;
import org.fruit.alayer.Pen;
import org.fruit.alayer.Point;
import org.fruit.alayer.Shape;
import org.fruit.alayer.State;
import org.fruit.alayer.SUT;
import org.fruit.alayer.Tag;
import org.fruit.alayer.Taggable;
import org.fruit.alayer.TaggableBase;
import org.fruit.alayer.Visualizer;
import org.fruit.alayer.Widget;
import org.fruit.alayer.Roles;
import org.fruit.alayer.StateBuildException;
import org.fruit.alayer.SystemStartException;
import org.fruit.alayer.Tags;
import org.fruit.alayer.Verdict;
import org.fruit.alayer.actions.NOP;
import org.fruit.alayer.devices.AWTMouse;
import org.fruit.alayer.devices.Mouse;
import org.fruit.alayer.devices.KBKeys;
import org.fruit.alayer.devices.MouseButtons;
import org.fruit.monkey.Main.LogLevel;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;
import org.jnativehook.mouse.NativeMouseEvent;
import org.jnativehook.mouse.NativeMouseListener;
import static org.fruit.monkey.Main.logln;
import static org.fruit.monkey.Main.log;

public abstract class AbstractProtocol implements NativeKeyListener, NativeMouseListener, UnProc<Settings> {

	public static enum Modes{
		Spy, Generate, GenerateDebug, Quit, View, Replay, ReplayDebug;
	}

	Set<KBKeys> pressed = EnumSet.noneOf(KBKeys.class);	
	private Settings settings;
	private Modes mode;
	private Mouse mouse = AWTMouse.build();
	private boolean saveStateSnapshot;
	int actionCount, sequenceCount;
	double startTime;

	protected void keyDown(KBKeys key){
		pressed.add(key);

		// state snapshot
		if(key == KBKeys.VK_UP && pressed.contains(KBKeys.VK_SHIFT))
			saveStateSnapshot = true;

		// change mode with shift + right (forward)
		if(key == KBKeys.VK_RIGHT && pressed.contains(KBKeys.VK_SHIFT))
			nextMode(true);

		// change mode with shift + left (backward)
		if(key == KBKeys.VK_LEFT && pressed.contains(KBKeys.VK_SHIFT))
			nextMode(false);

		// quit with shift + down
		if(key == KBKeys.VK_DOWN && pressed.contains(KBKeys.VK_SHIFT)){
			logln("User requested to stop monkey!", LogLevel.Info);
			mode = Modes.Quit;
		}

		// toggle action visualization
		if(key == KBKeys.VK_1 && pressed.contains(KBKeys.VK_SHIFT))
			settings().set(ConfigTags.VisualizeActions, !settings().get(ConfigTags.VisualizeActions));		

		// toggle widget mark visualization
		if(key == KBKeys.VK_2 && pressed.contains(KBKeys.VK_SHIFT))
			settings().set(ConfigTags.DrawWidgetUnderCursor, !settings().get(ConfigTags.DrawWidgetUnderCursor));		

		// toggle widget info visualization
		if(key == KBKeys.VK_3 && pressed.contains(KBKeys.VK_SHIFT))
			settings().set(ConfigTags.DrawWidgetInfo, !settings().get(ConfigTags.DrawWidgetInfo));		
	}

	protected void keyUp(KBKeys key){ pressed.remove(key); }
	protected void mouseDown(MouseButtons btn, double x, double y){}
	protected void mouseUp(MouseButtons btn, double x, double y){}

	public final void nativeKeyPressed(NativeKeyEvent e) {
		for(KBKeys key : KBKeys.values())
			if(key.code() == e.getKeyCode())
				keyDown(key);
	}

	public final void nativeKeyReleased(NativeKeyEvent e) {
		for(KBKeys key : KBKeys.values())
			if(key.code() == e.getKeyCode())
				keyUp(key);
	}

	public final void nativeMouseClicked(NativeMouseEvent arg0) {}

	public final void nativeMousePressed(NativeMouseEvent arg0) {
		if(arg0.getButton() == 1)
			mouseDown(MouseButtons.BUTTON1, arg0.getX(), arg0.getY());
		else if(arg0.getButton() == 2)
			mouseDown(MouseButtons.BUTTON3, arg0.getX(), arg0.getY());
		else if(arg0.getButton() == 3)
			mouseDown(MouseButtons.BUTTON2, arg0.getX(), arg0.getY());
	}

	public final void nativeMouseReleased(NativeMouseEvent arg0) {
		if(arg0.getButton() == 1)
			mouseUp(MouseButtons.BUTTON1, arg0.getX(), arg0.getY());
		else if(arg0.getButton() == 2)
			mouseUp(MouseButtons.BUTTON3, arg0.getX(), arg0.getY());
		else if(arg0.getButton() == 3)
			mouseUp(MouseButtons.BUTTON2, arg0.getX(), arg0.getY());
	}

	public final void nativeKeyTyped(NativeKeyEvent e) {}
	public synchronized Modes mode(){ return mode; }

	private synchronized void nextMode(boolean forward){

		if(forward){
			switch(mode){
			case Spy: mode = Modes.Generate; break;
			case Generate: mode = Modes.GenerateDebug; break;
			case GenerateDebug: mode = Modes.Spy; break;
			case Replay: mode = Modes.ReplayDebug; break;
			case ReplayDebug: mode = Modes.Replay; break;
			default: break;
			}		
		}else{
			switch(mode){
			case Spy: mode = Modes.GenerateDebug; break;
			case Generate: mode = Modes.Spy; break;
			case GenerateDebug: mode = Modes.Generate; break;
			case Replay: mode = Modes.ReplayDebug; break;
			case ReplayDebug: mode = Modes.Replay; break;
			default: break;
			}			
		}
		logln("'" + mode + "' mode active.", LogLevel.Info);
	}

	protected final double timeElapsed(){ return Util.time() - startTime; }
	protected final Settings settings(){ return settings; }
	protected void beginSequence() {}
	protected void finishSequence(File recordedSequence) {}
	protected abstract SUT startSystem() throws SystemStartException;
	protected abstract State getState(SUT system) throws StateBuildException;
	protected abstract Set<Action> deriveActions(SUT system, State state) throws ActionBuildException;
	protected abstract Action selectAction(State state, Set<Action> actions);
	protected abstract Canvas buildCanvas();
	protected abstract boolean moreActions(State state);
	protected abstract boolean moreSequences();
	protected final int actionCount(){ return actionCount; }
	protected final int sequenceCount(){ return sequenceCount; }
	protected void initialize(Settings settings){}

	private void visualizeState(Canvas canvas, State state){
		if((mode() == Modes.Spy || mode() == Modes.ReplayDebug) && settings().get(ConfigTags.DrawWidgetUnderCursor)){
			Point cursor = mouse.cursor();
			Widget cursorWidget = Util.widgetFromPoint(state, cursor.x(), cursor.y(), null);

			if(cursorWidget != null){
				Shape cwShape = cursorWidget.get(Tags.Shape, null);
				if(cwShape != null){
					Pen mark = Pen.newPen().setColor(Color.from(0, 255, 0, 100)).setFillPattern(FillPattern.Solid).build();
					cwShape.paint(canvas, mark);

					Pen rpen = Pen.newPen().setColor(Color.Red).build();
					Pen apen = Pen.newPen().setColor(Color.Black).build();
					Pen wpen = Pen.newPen().setColor(Color.from(255, 255, 255, 170)).setFillPattern(FillPattern.Solid).build();
					canvas.text(rpen, cwShape.x(), cwShape.y(), 0, "Role: " + cursorWidget.get(Role, Roles.Widget).toString());
					canvas.text(rpen, cwShape.x(), cwShape.y() - 20, 0, Util.indexString(cursorWidget));

					if(settings().get(ConfigTags.DrawWidgetInfo)){
						canvas.rect(wpen, cwShape.x(), cwShape.y() - 20, 550, Util.size(cursorWidget.tags()) * 25);
						canvas.rect(apen, cwShape.x(), cwShape.y() - 20, 550, Util.size(cursorWidget.tags()) * 25);
						canvas.text(rpen, cwShape.x(), cwShape.y(), 0, "Role: " + cursorWidget.get(Role, Roles.Widget).toString());
						canvas.text(rpen, cwShape.x(), cwShape.y() - 20, 0, Util.indexString(cursorWidget));
						int pos = 20;
						StringBuilder sb = new StringBuilder();
						sb.append("Ancestors: ");
						for(Widget p : Util.ancestors(cursorWidget))
							sb.append("::").append(p.get(Role, Roles.Widget));							
						canvas.text(apen, cwShape.x(), cwShape.y() + (pos+=20), 0, sb.toString());
						pos += 20;
						for(Tag<?> t : cursorWidget.tags()){
							canvas.text((t.equals(Tags.Title) || t.equals(Tags.Role)) ? rpen : apen, cwShape.x(), cwShape.y() + (pos+=20), 0, t.name() + ":   " + Util.abbreviate(Util.toString(cursorWidget.get(t)), 50, "..."));
						}
					}
				}
			}
		}
	}

	private void visualizeActions(Canvas canvas, State state, Set<Action> actions){
		if((mode() == Modes.Spy || mode() == Modes.GenerateDebug) && settings().get(ConfigTags.VisualizeActions)){
			for(Action a : actions)
				a.get(Visualizer, Util.NullVisualizer).run(state, canvas, Pen.IgnorePen);
		}
	}

	private void visualizeSelectedAction(Canvas canvas, State state, Action action){
		if(mode() == Modes.GenerateDebug || mode() == Modes.ReplayDebug){
			Pen redPen = Pen.newPen().setColor(Color.Red).setFillPattern(FillPattern.Solid).setStrokeWidth(20).build();
			Visualizer visualizer = action.get(Visualizer, Util.NullVisualizer);

			final int BLINK_COUNT = 3;
			final double BLINK_DELAY = 0.5;
			for(int i = 0; i < BLINK_COUNT; i++){
				Util.pause(BLINK_DELAY);
				canvas.begin();
				visualizer.run(state, canvas, Pen.IgnorePen);
				canvas.end();
				Util.pause(BLINK_DELAY);
				canvas.begin();
				visualizer.run(state, canvas, redPen);
				canvas.end();
			}
		}
	}

	protected boolean executeAction(SUT system, State state, Action action){
		try{
			action.run(system, state, settings.get(ConfigTags.ActionDuration));
			Util.pause(settings.get(ConfigTags.TimeToWaitAfterAction));
			return true;
		}catch(ActionFailedException afe){
			return false;
		}
	}

	private void saveStateSnapshot(State state){
		try{
			if(saveStateSnapshot){
				//System.out.println(Utils.treeDesc(state, 2, Tags.Role, Tags.Desc, Tags.Shape, Tags.Blocked));
				Taggable taggable = new TaggableBase();
				taggable.set(SystemState, state);
				logln("Saving state snapshot...", LogLevel.Debug);
				File file = Util.generateUniqueFile(settings.get(ConfigTags.OutputDir), "state_snapshot");
				ObjectOutputStream oos = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
				oos.writeObject(taggable);
				oos.close();
				saveStateSnapshot = false;
				logln("Saved state snapshot to " + file.getAbsolutePath(), LogLevel.Info);
			}
		}catch(IOException ioe){
			throw new RuntimeException(ioe);
		}
	}

	public final void run(final Settings settings) {
		startTime = Util.time();
		this.settings = settings;
		mode = settings.get(ConfigTags.Mode);
		ObjectOutputStream oos = null;
		boolean problems = false;
		initialize(settings);

		try {
			logln("Registering keyboard and mouse hooks", LogLevel.Debug);
			GlobalScreen.registerNativeHook();
			GlobalScreen.getInstance().addNativeKeyListener(this);
			GlobalScreen.getInstance().addNativeMouseListener(this);
			logln("Successfully registered keyboard and mouse hooks!", LogLevel.Debug);

			logln("'" + mode() + "' mode active.", LogLevel.Info);

			if(mode() == Modes.View){
				new SequenceViewer(settings).run();
			}else if(mode() == Modes.Replay || mode() == Modes.ReplayDebug){
				replay();
			}else if(mode() == Modes.Generate || mode() == Modes.Spy || mode() == Modes.GenerateDebug){

				while(mode() != Modes.Quit && moreSequences()){
					problems = false;
					actionCount = 0;

					logln("Creating new sequence file...", LogLevel.Debug);
					final File currentSeq = new File(settings.get(ConfigTags.TempDir) + File.separator + "tmpsequence");
					Util.delete(currentSeq);
					//oos = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(currentSeq), 50000000));
					RandomAccessFile raf = new RandomAccessFile(currentSeq, "rw");
					oos = new ObjectOutputStream(new FileOutputStream(raf.getFD()));
					logln("Created new sequence file!", LogLevel.Debug);

					logln("Building canvas...", LogLevel.Debug);
					Canvas cv = buildCanvas();
					logln(Util.dateString("dd.MMMMM.yyyy HH:mm:ss") + " Starting system...", LogLevel.Info);
					SUT system = startSystem();
					//SUT system = WinProcess.fromProcName("firefox.exe");
					logln("System is running!", LogLevel.Debug);
					logln("Starting sequence " + sequenceCount, LogLevel.Info);
					beginSequence();
					logln("Obtaining system state...", LogLevel.Debug);
					State state = getState(system);
					logln("Successfully obtained system state!", LogLevel.Debug);
					saveStateSnapshot(state);
					Verdict verdict = state.get(OracleVerdict, Verdict.OK); 
					if(verdict.severity() >= settings().get(ConfigTags.FaultThreshold)){
						problems = true;					
						logln("Detected fault: " + verdict, LogLevel.Critical);
					}
					Taggable fragment = new TaggableBase();
					fragment.set(SystemState, state);

					while(mode() != Modes.Quit && moreActions(state)){
						boolean actionSucceeded = true;
						cv.begin();
						Util.clear(cv);
						visualizeState(cv, state);
						logln("Building action set...", LogLevel.Debug);
						Set<Action> actions = deriveActions(system, state);
						
						if(actions.isEmpty()){
							logln("No available actions to execute! Stopping sequence generation!", LogLevel.Critical);
							problems = true;
							break;
						}
						
						fragment.set(ActionSet, actions);
						logln("Built action set!", LogLevel.Debug);
						visualizeActions(cv, state, actions);
						cv.end();

						if(mode() == Modes.Quit) break;
						logln("Selecting action...", LogLevel.Debug);
						Action action = selectAction(state, actions);
						logln("Selected action '" + action + "'.", LogLevel.Debug);

						visualizeSelectedAction(cv, state, action);
						if(mode() == Modes.Quit) break;

						if(mode() != Modes.Spy){
							logln(String.format("Executing (%d): %s...", actionCount, action.get(Desc, action.toString())), LogLevel.Debug);
							if((actionSucceeded = executeAction(system, state, action))){							
								logln(String.format("Executed (%d): %s...", actionCount, action.get(Desc, action.toString())), LogLevel.Info);
								actionCount++;
								fragment.set(ExecutedAction, action);
								fragment.set(ActionDuration, settings().get(ConfigTags.ActionDuration));
								fragment.set(ActionDelay, settings().get(ConfigTags.TimeToWaitAfterAction));
								logln("Writing fragment to sequence file...", LogLevel.Debug);
								oos.writeObject(fragment);

								if(actionCount % 4 == 0){
									oos.reset();
									oos.flush();
								}

								logln("Wrote fragment to sequence file!", LogLevel.Debug);
							}else{
								logln("Excecution of action failed!");
							}
						}

						if(mode() == Modes.Quit) break;
						if(!actionSucceeded){
							problems = true;
							break;
						}
						
						logln("Obtaining system state...", LogLevel.Debug);
						state = getState(system);
						logln("Successfully obtained system state!", LogLevel.Debug);
						saveStateSnapshot(state);
						verdict = state.get(OracleVerdict, Verdict.OK); 
						if(verdict.severity() >= settings().get(ConfigTags.FaultThreshold)){
							problems = true;
							logln("Detected fault: " + verdict, LogLevel.Critical);
						}
						fragment = new TaggableBase();
						fragment.set(SystemState, state);
					}
					logln("Writing fragment to sequence file...", LogLevel.Debug);
					oos.writeObject(fragment);
					logln("Wrote fragment to sequence file!", LogLevel.Debug);
					oos.close();
					raf.close();
					logln("Sequence " + sequenceCount + " finished.", LogLevel.Info);
					if(problems)
						logln("Sequence contained problems!", LogLevel.Critical);
					finishSequence(currentSeq);
					if(problems || !settings().get(ConfigTags.OnlySaveFaultySequences)){
						String generatedSequence = Util.generateUniqueFile(settings.get(ConfigTags.OutputDir), "sequence").getName();
						logln("Copying generated sequence (\"" + settings.get(ConfigTags.OutputDir) + File.separator + generatedSequence + "\") to output directory...", LogLevel.Info);
						Util.copyToDirectory(currentSeq.getAbsolutePath(), 
								settings.get(ConfigTags.OutputDir), 
								generatedSequence);
						logln("Copied generated sequence to output directory!", LogLevel.Debug);
					}

					logln("Releasing canvas...", LogLevel.Debug);
					cv.release();
					logln("Shutting down system...", LogLevel.Info);
					system.stop();
					logln("System has been shut down!", LogLevel.Debug);
					sequenceCount++;
				}
			}
		} catch (NativeHookException e) {
			logln("Unable to install keyboard and mouse hooks!", LogLevel.Critical);
			throw new RuntimeException("Unable to install keyboard and mouse hooks!", e);
		}catch(IOException ioe){
			logln("Unable to create sequence file or to save action!", LogLevel.Critical);
			throw new RuntimeException(ioe);
		}finally{
			try{
				if(oos != null)
					oos.close();
			}catch(Exception e){}
			try{
				logln("Unregistering keyboard and mouse hooks", LogLevel.Debug);
				GlobalScreen.unregisterNativeHook();
			}catch(Exception e){}
		}
	}

	private void replay(){
		boolean success = true;
		actionCount = 0;

		try{
			File seqFile = new File(settings.get(ConfigTags.PathToReplaySequence));
			ObjectInputStream ois = new ObjectInputStream(new BufferedInputStream(new FileInputStream(seqFile)));

			SUT system = startSystem();
			Canvas cv = buildCanvas();
			State state = getState(system);

			while(success && mode() != Modes.Quit){
				Taggable fragment;
				try{
					fragment = (Taggable) ois.readObject();
				}catch(IOException ioe){
					success = true;
					break;
				}

				success = false;
				int tries = 0;
				double start = Util.time();

				while(!success && (Util.time() - start < settings.get(ConfigTags.ReplayRetryTime))){
					tries++;
					cv.begin();
					Util.clear(cv);
					visualizeState(cv, state);
					cv.end();

					if(mode() == Modes.Quit) break;
					Action action = fragment.get(ExecutedAction, new NOP());
					visualizeSelectedAction(cv, state, action);
					if(mode() == Modes.Quit) break;

					double actionDuration = settings.get(ConfigTags.UseRecordedActionDurationAndWaitTimeDuringReplay) ? fragment.get(Tags.ActionDuration, 0.0) : settings.get(ConfigTags.ActionDuration);
					double actionDelay = settings.get(ConfigTags.UseRecordedActionDurationAndWaitTimeDuringReplay) ? fragment.get(Tags.ActionDelay, 0.0) : settings.get(ConfigTags.TimeToWaitAfterAction);

					try{
						if(tries < 2){
							log(String.format("Trying to execute (%d): %s...", actionCount, action.get(Desc, action.toString())), LogLevel.Info);
						}else{
							if(tries % 50 == 0)
								logln(".", LogLevel.Info);
							else
								log(".", LogLevel.Info);
						}

						action.run(system, state, actionDuration);
						success = true;
						actionCount++;
						logln("Success!", LogLevel.Info);
					}catch(ActionFailedException afe){}

					Util.pause(actionDelay);

					if(mode() == Modes.Quit) break;
					state = getState(system);
				}

			}

			cv.release();
			ois.close();

		}catch(IOException ioe){
			throw new RuntimeException("Cannot read file.", ioe);
		}catch (ClassNotFoundException cnfe) {
			throw new RuntimeException("Cannot read file.", cnfe);
		}

		if(success)
			logln("Sequence successfully replayed!", LogLevel.Info);
		else
			logln("Failed to replay sequence.", LogLevel.Critical);
	}
}
