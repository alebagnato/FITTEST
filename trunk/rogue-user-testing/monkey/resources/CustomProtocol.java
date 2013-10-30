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
import static org.fruit.alayer.windows.UIARoles.*;
import java.io.File;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.fruit.Assert;
import org.fruit.Pair;
import org.fruit.Util;
import org.fruit.alayer.Action;
import org.fruit.alayer.ActionBuildException;
import org.fruit.alayer.ActionFailedException;
import org.fruit.alayer.Color;
import org.fruit.alayer.FillPattern;
import org.fruit.alayer.Pen;
import org.fruit.alayer.Role;
import org.fruit.alayer.Roles;
import org.fruit.alayer.SUT;
import org.fruit.alayer.ShapeVisualizer;
import org.fruit.alayer.State;
import org.fruit.alayer.StateBuildException;
import org.fruit.alayer.StrokePattern;
import org.fruit.alayer.SystemStartException;
import org.fruit.alayer.Verdict;
import org.fruit.alayer.Visualizer;
import org.fruit.alayer.Widget;
import org.fruit.alayer.actions.AnnotatingActionCompiler;
import org.fruit.alayer.actions.StdActionCompiler;
import org.fruit.alayer.devices.KBKeys;
import static org.fruit.alayer.windows.UIATags.*;
import static org.fruit.monkey.ConfigTags.*;
import org.fruit.monkey.DefaultProtocol;
import org.fruit.monkey.Settings;
import org.fruit.alayer.Tags;
import static org.fruit.alayer.Tags.NotResponding;
import static org.fruit.alayer.Tags.IsRunning;
import static org.fruit.alayer.Tags.RunningProcesses;
import static org.fruit.alayer.Tags.SystemActivator;
import static org.fruit.alayer.Tags.Blocked;
import static org.fruit.alayer.Tags.Title;
import static org.fruit.alayer.Tags.Foreground;
import static org.fruit.alayer.Tags.Enabled;


public class CustomProtocol extends DefaultProtocol {

	/** 
	 * Called once during the life time of the Rogue User
	 * This method can be used to perform initial setup work
	 * @param   settings   the current Rogue User settings as specified by the user.
	 */
	protected void initialize(Settings settings){
		super.initialize(settings);
	}
	
	/**
	 * This method is invoked each time the Rogue User starts to generate a new sequence
	 */
	protected void beginSequence(){
		super.beginSequence();
	}
	
	/**
	 * This method is called when the Rogue User starts the System Under Test (SUT). The method should
	 * take care of 
	 *   1) starting the SUT (you can use the Rogue User's settings obtainable from <code>settings()</code> to find
	 *      out what executable to run)
	 *   2) bringing the system into a specific start state which is identical on each start (e.g. one has to delete or restore
	 *      the SUT's configuratio files etc.)
	 *   3) waiting until the system is fully loaded and ready to be tested (with large systems, you might have to wait several
	 *      seconds until they have finished loading)
     * @return  a started SUT, ready to be tested.
	 */
	protected SUT startSystem() throws SystemStartException{
		return super.startSystem();
	}

	/**
	 * This method is called when the Rogue User requests the state of the SUT.
	 * Here you can add additional information to the SUT's state or write your
	 * own state fetching routine. The state should have attached an oracle 
	 * (TagName: <code>Tags.OracleVerdict</code>) which describes whether the 
	 * state is erroneous and if so why.
	 * @return  the current state of the SUT with attached oracle.
	 */
	protected State getState(SUT system) throws StateBuildException{
		return super.getState(system);
	}

	/**
	 * This is a helper method used by the default implementation of <code>buildState()</code>
	 * It examines the SUT's current state and returns an oracle verdict.
	 * @return oracle verdict, which determines whether the state is erroneous and why.
	 */
	protected Verdict getVerdict(State state){
		Assert.notNull(state);

		//-------------------
		// ORACLES FOR FREE
		//-------------------

		// if the SUT is not running, we assume it crashed
		if(!state.get(IsRunning, false))
			return new Verdict(1.0, "System is offline! I assume it crashed!");

		// if the SUT does not respond within a given amount of time, we assume it crashed
		if(state.get(NotResponding, false))
			return new Verdict(0.8, "System is unresponsive! I assume something is wrong!");

        //------------------------
		// ORACLES ALMOST FOR FREE
        //------------------------

		String titleRegEx = settings().get(SuspiciousTitles);
		
		// search all widgets for suspicious titles
		for(Widget w : state){
			String title = w.get(Title, "");
			if(title.matches(titleRegEx)){
				Visualizer visualizer = Util.NullVisualizer;
				
				// visualize the problematic widget, by marking it with a red box
				if(w.get(Tags.Shape, null) != null){
					Pen redPen = Pen.newPen().setColor(Color.Red).setFillPattern(FillPattern.None).setStrokePattern(StrokePattern.Solid).build();
					visualizer = new ShapeVisualizer(redPen, w.get(Tags.Shape), "Suspicious Title", 0.5, 0.5);
				}
				return new Verdict(1.0, "Discovered suspicious widget title: '" + title + "'.", visualizer);
			}
		}
		
		//-----------------------------------------------------------------------------
		// MORE SOPHISTICATED ORACLES CAN BE PROGRAMMED HERE (the sky is the limit ;-)
        //-----------------------------------------------------------------------------

		
		// if everything was ok...
		return Verdict.OK;
	}

	/**
	 * This method is used by the Rogue User to determine the set of currently available actions.
	 * You can use the SUT's current state, analyze the widgets and their properties to create
	 * a set of sensible actions, such as: "Click every Button which is enabled" etc.
	 * The return value is supposed to be non-null. If the returned set is empty, the Rogue User
	 * will stop generation of the current action and continue with the next one.
	 * @param system the SUT
	 * @param state the SUT's current state
	 * @return  a set of actions
	 */
	protected Set<Action> deriveActions(SUT system, State state) throws ActionBuildException{
		Assert.notNull(state);
		Set<Action> actions = Util.newHashSet();
		
		// create an action compiler, which helps us create actions, such as clicks, drag + drop, typing...
		StdActionCompiler ac = new AnnotatingActionCompiler();

		// if there is an unwanted process running, kill it
		String processRE = settings().get(ProcessesToKillDuringTest);
		for(Pair<Long, String> process : state.get(RunningProcesses, Collections.<Pair<Long, String>>emptyList())){
			if(process.right() != null && process.right().matches(processRE)){
				actions.add(ac.killProcessByName(process.right(), 2));
				return actions;
			}
		}
		
		// if the system is in the background force it into the foreground!
		if(!state.get(Foreground, true) && system.get(SystemActivator, null) != null){
			actions.add(ac.activateSystem());
			return actions;
		}

		// iterate through all widgets
		for(Widget w : state){
			if(w.get(Enabled, true) && !w.get(Blocked, false)){ // only consider enabled and non-blocked widgets
				// create left clicks
				if(isClickable(w))
					actions.add(ac.leftClickAt(w));

				// type into text boxes
				if(isTypeable(w))
					actions.add(ac.clickTypeInto(w, "Test Input 1234"));
			}
		}

		// if we did not find any actions, then we just hit escape, maybe that works ;-)
		if(actions.isEmpty())
			actions.add(ac.hitKey(KBKeys.VK_ESCAPE));

		return actions;
	}

	/**
	 * Select one of the possible actions (e.g. at random)
	 * @param state the SUT's current state
	 * @param actions the set of available actions as computed by <code>buildActionsSet()</code>
	 * @return  the selected action (non-null!)
	 */
	protected Action selectAction(State state, Set<Action> actions){ 
		return super.selectAction(state, actions);
	}

	/**
	 * Execute the selected action.
	 * @param system the SUT
	 * @param state the SUT's current state
	 * @param action the action to execute
	 * @return whether or not the execution succeeded
	 */
	protected boolean executeAction(SUT system, State state, Action action){
		return super.executeAction(system, state, action);
	}
	
	/**
	 * The Rogue User uses this method to determine when to stop the generation of actions for the
	 * current sequence. You could stop the sequence's generation after a given amount of executed
	 * actions or after a specific time etc.
	 * @return  if <code>true</code> continue generation, else stop
	 */
	protected boolean moreActions(State state) {
		return super.moreActions(state);
	}


	/** 
	 * This method is invoked each time after the Rogue User finished the generation of a sequence.
	 */
	protected void finishSequence(File recordedSequence){
		super.finishSequence(recordedSequence);
	}


	/**
	 * The Rogue User uses this method to determine when to stop the entire test.
	 * You could stop the test after a given amount of generated sequences or
	 * after a specific time etc.
	 * @return  if <code>true</code> continue test, else stop	 */
	protected boolean moreSequences() {
		return super.moreSequences();
	}
	
	
	private boolean isClickable(Widget w){
		Role role = w.get(Tags.Role, Roles.Widget);
		
		if(!Role.isOneOf(role, UIASeparator, UIAToolBar, UIAToolTip, UIAMenuBar, 
				UIAMenu, UIAHeader, UIATabControl, UIAPane, UIATree, UIAWindow, 
				UIATitleBar, UIAThumb, UIAEdit, UIAText)){
			String title = w.get(Title, "");

			if(!title.matches(settings().get(ClickFilter))){					
				if(Util.hitTest(w, 0.5, 0.5))
					return true;
			}
		}
		return false;
	}
	
	private boolean isTypeable(Widget w){
		Role role = w.get(Tags.Role, Roles.Widget);

		if(Role.isOneOf(role, UIAEdit, UIAText) && w.get(UIAIsKeyboardFocusable)){
			if(Util.hitTest(w, 0.5, 0.5))
				return true;
		}
		return false;
	}
}
