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
import static org.fruit.alayer.Tags.Blocked;
import static org.fruit.alayer.Tags.Enabled;
import static org.fruit.alayer.Tags.Title;
import static org.fruit.alayer.windows.UIARoles.UIAEdit;
import static org.fruit.alayer.windows.UIARoles.UIAHeader;
import static org.fruit.alayer.windows.UIARoles.UIAMenu;
import static org.fruit.alayer.windows.UIARoles.UIAMenuBar;
import static org.fruit.alayer.windows.UIARoles.UIAPane;
import static org.fruit.alayer.windows.UIARoles.UIASeparator;
import static org.fruit.alayer.windows.UIARoles.UIATabControl;
import static org.fruit.alayer.windows.UIARoles.UIAText;
import static org.fruit.alayer.windows.UIARoles.UIAThumb;
import static org.fruit.alayer.windows.UIARoles.UIATitleBar;
import static org.fruit.alayer.windows.UIARoles.UIAToolBar;
import static org.fruit.alayer.windows.UIARoles.UIAToolTip;
import static org.fruit.alayer.windows.UIARoles.UIATree;
import static org.fruit.alayer.windows.UIARoles.UIAWindow;
import java.util.Collections;
import java.util.Set;
import org.fruit.Assert;
import org.fruit.Pair;
import org.fruit.Util;
import org.fruit.alayer.ActionBuildException;
import org.fruit.alayer.Action;
import org.fruit.alayer.Role;
import org.fruit.alayer.SUT;
import org.fruit.alayer.State;
import org.fruit.alayer.Widget;
import org.fruit.alayer.Roles;
import org.fruit.alayer.Tags;
import org.fruit.alayer.actions.AnnotatingActionCompiler;
import org.fruit.alayer.actions.NOP;
import org.fruit.alayer.actions.StdActionCompiler;
import org.fruit.alayer.devices.KBKeys;
import org.fruit.alayer.windows.UIARoles;
import org.fruit.alayer.windows.UIATags;
import org.fruit.monkey.ConfigTags;
import org.fruit.monkey.DefaultProtocol;

public class ModelioProtocol extends DefaultProtocol {

	protected Set<Action> buildActionSet(SUT system, State state) throws ActionBuildException{
		Assert.notNull(state);
		Set<Action> actions = Util.newHashSet();	
		StdActionCompiler ac = new AnnotatingActionCompiler();

		// if we have an unwanted process, kill it
		String processRE = settings().get(ConfigTags.ProcessesToKillDuringTest);
		for(Pair<Long, String> process : state.get(Tags.RunningProcesses, Collections.<Pair<Long, String>>emptyList())){
			if(process.right() != null && process.right().matches(processRE)){
				actions.add(ac.killProcessByName(process.right(), 2));
				return actions;
			}
		}

		
		// if the system is in the background force it into the foreground!
		if(!state.get(Tags.Foreground, true) && system.get(Tags.SystemActivator, null) != null){
			if(!settings().get(ConfigTags.ForceForeground))
				actions.add(new NOP());
			else
				actions.add(ac.activateSystem());
			return actions;
		}


		Widget cteTree = null;

		for(Widget w : state){
			if(w.get(Tags.Role, Roles.Widget).isA(UIAText)){
				if(w.get(Title, "").equals("CteTree")){
					cteTree = w;
					break;
				}
			}
		}


		for(Widget w : state){
			Role role = w.get(Tags.Role, Roles.Widget);

			// left clicks
			if(w.get(Enabled, true) && !w.get(Blocked, false)){
				if(!Role.isOneOf(role, UIASeparator, UIAToolBar, UIAToolTip, UIAMenuBar, 
						UIAMenu, UIAHeader, UIATabControl, UIAPane, UIATree, UIAWindow, 
						UIATitleBar, UIAThumb, UIAEdit, UIAText)){
					String title = w.get(Title, "");

					if(!title.matches(settings().get(ConfigTags.ClickFilter))){
						if(Util.hitTest(w, 0.5, 0.5)){
							actions.add(ac.leftClickAt(w));
						}							
					}
				}
			}

			// right clicks
			if(w.get(Enabled, true) && !w.get(Blocked, false)){
				if(Role.isOneOf(role, UIARoles.UIATree, UIARoles.UIATreeItem)){
					if(Util.hitTest(w, 0.5, 0.5)){
						actions.add(ac.rightClickAt(w));
					}
				}
			}

			// double clicks
			if(w.get(Enabled, true) && !w.get(Blocked, false)){
				if(Role.isOneOf(role, UIARoles.UIATree, UIARoles.UIATreeItem)){
					if(Util.hitTest(w, 0.5, 0.5)){
						actions.add(ac.leftDoubleClickAt(w));
					}
				}
			}

			// typing
			if(w.get(Enabled, true) && !w.get(Blocked, false)){
				if(Role.isOneOf(role, UIAEdit, UIAText) && (w.get(UIATags.UIAIsKeyboardFocusable, false) || w.get(UIATags.UIAHasKeyboardFocus, false)) && !(w.get(Title, "").matches("CteTree"))){
					if(Util.hitTest(w, 0.5, 0.5)){
						actions.add(ac.clickTypeInto(w, "1234"));
					}
				}
			}

			// drag drop for the cte tree items
			if(cteTree != null && w.get(Enabled, true) && !w.get(Blocked, false)){
				if((w.get(Title, "").matches("Cte(Classification|Composition|Class)"))){
					if(Util.hitTest(w, 0.5, 0.5)){
						if(Util.hitTest(cteTree, 0.5, 0.5)){
							actions.add(ac.dragFromTo(w, 0.5, 0.5, cteTree, 0.5, 0.5));
						}
						if(Util.hitTest(cteTree, 0.15, 0.15)){
							actions.add(ac.dragFromTo(w, 0.5, 0.5, cteTree, 0.15, 0.15));
						}
					}
				}
			}
		}

		// if we did not find any actions, then we just hit escape
		if(actions.isEmpty())
			actions.add(ac.hitKey(KBKeys.VK_ESCAPE));

		return actions;
	}
}
