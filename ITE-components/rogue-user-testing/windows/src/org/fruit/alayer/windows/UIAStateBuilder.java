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
package org.fruit.alayer.windows;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.fruit.Assert;
import org.fruit.alayer.Rect;
import org.fruit.alayer.Roles;
import org.fruit.alayer.StateBuilder;
import org.fruit.alayer.SUT;
import org.fruit.alayer.StateBuildException;
import org.fruit.alayer.Tags;

public final class UIAStateBuilder implements StateBuilder {

	private static final long serialVersionUID = 796655140981849818L;
	final double timeOut;
	transient ExecutorService executor;
	transient long pAutomation, pCondition, pCacheRequest;

	public UIAStateBuilder(){ this(10);	}

	public UIAStateBuilder(double timeOut){
		Assert.isTrue(timeOut > 0);
		this.timeOut = timeOut;
		initialize();
		executor = Executors.newFixedThreadPool(1);
	}

	private void initialize(){
		Windows.CoInitializeEx(0, Windows.COINIT_MULTITHREADED);
		System.out.println(Windows.Get_CLSID_CUIAutomation_Ptr());
		pAutomation = Windows.CoCreateInstance(Windows.Get_CLSID_CUIAutomation_Ptr(), 0, Windows.CLSCTX_INPROC_SERVER, Windows.Get_IID_IUIAutomation_Ptr());

		// scope and filter settings
		//long pFirstCondition = Windows.IUIAutomation_CreateTrueCondition(pAutomation);
		long pFirstCondition = Windows.IUIAutomation_get_ControlViewCondition(pAutomation);	


		//pCondition = Windows.IUIAutomation_CreateTrueCondition(pAutomation);
		pCondition = Windows.IUIAutomation_CreateAndCondition(pAutomation, Windows.IUIAutomation_CreatePropertyCondition(pAutomation, Windows.UIA_IsOffscreenPropertyId, false), pFirstCondition);
		//Windows.IUnknown_Release(pFirstCondition);

		pCacheRequest = Windows.IUIAutomation_CreateCacheRequest(pAutomation);
		Windows.IUIAutomationCacheRequest_put_TreeFilter(pCacheRequest, pCondition);
		Windows.IUIAutomationCacheRequest_put_TreeScope(pCacheRequest, Windows.TreeScope_Subtree);
		Windows.IUIAutomationCacheRequest_put_AutomationElementMode(pCacheRequest, Windows.AutomationElementMode_Full);


		// cache patterns
		Windows.IUIAutomationCacheRequest_AddPattern(pCacheRequest, Windows.UIA_WindowPatternId);
		Windows.IUIAutomationCacheRequest_AddPattern(pCacheRequest, Windows.UIA_ValuePatternId);


		// cache properties
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_NamePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_AutomationIdPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_BoundingRectanglePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ClassNamePropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ClickablePointPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ControllerForPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ControlTypePropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_CulturePropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_DescribedByPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_FlowsToPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_FrameworkIdPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_HasKeyboardFocusPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_HelpTextPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsContentElementPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsControlElementPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsDataValidForFormPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsEnabledPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsKeyboardFocusablePropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsOffscreenPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsPasswordPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsRequiredForFormPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ItemStatusPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ItemTypePropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_LabeledByPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_LocalizedControlTypePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_NativeWindowHandlePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_ProviderDescriptionPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_OrientationPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_RuntimeIdPropertyId);
		//Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_IsWindowPatternAvailablePropertyId);

		// window role properties
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowIsTopmostPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowCanMaximizePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowCanMinimizePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowIsModalPropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowWindowInteractionStatePropertyId);
		Windows.IUIAutomationCacheRequest_AddProperty(pCacheRequest, Windows.UIA_WindowWindowVisualStatePropertyId);
	}

	public void release(){
		if(pAutomation != 0){
			Windows.IUnknown_Release(pCondition);
			Windows.IUnknown_Release(pCacheRequest);
			Windows.IUnknown_Release(pAutomation);
			Windows.CoUninitialize();
			pAutomation = 0;
			executor.shutdown();
		}
	}

	public void finalize(){ release(); }

	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException{
		ois.defaultReadObject();
		initialize();
		executor = Executors.newFixedThreadPool(1);
	}

	private void writeObject(ObjectOutputStream oos) throws IOException{
		oos.defaultWriteObject();
	}

	private class StateFetcher implements Callable<UIAState>{
		private final SUT system;

		public StateFetcher(SUT system){ 
			this.system = system;
		}

		public UIAState call() throws Exception {
			Windows.CoInitializeEx(0, Windows.COINIT_MULTITHREADED);

			UIARootElement uiaRoot = buildSkeletton(system);
			UIAState root = createWidgetTree(uiaRoot);
			root.set(Tags.Role, Roles.Process);
			root.set(Tags.NotResponding, false);

			Windows.CoUninitialize();
			return root;
		}

		UIARootElement buildSkeletton(SUT system){
			UIARootElement uiaRoot = new UIARootElement();	
			uiaRoot.isRunning = system.isRunning();

			long[] info = Windows.GetMonitorInfo(Windows.GetPrimaryMonitorHandle());
			uiaRoot.rect = Rect.fromCoordinates(info[1], info[2], info[3], info[4]);
			uiaRoot.timeStamp = System.currentTimeMillis();
			uiaRoot.hasStandardKeyboard = system.get(Tags.StandardKeyboard, null) != null;
			uiaRoot.hasStandardMouse = system.get(Tags.StandardMouse, null) != null;

			if(!uiaRoot.isRunning)
				return uiaRoot;

			uiaRoot.pid = system.get(Tags.PID);
			uiaRoot.isForeground = WinProcess.isForeground(uiaRoot.pid);			

			// find all visible top level windows on the desktop
			Iterable<Long> visibleTopLevelWindows = visibleTopLevelWindows();

			// descend the root windows which belong to our process, using UIAutomation
			uiaRoot.children = new ArrayList<UIAElement>();
			List<Long> ownedWindows = new ArrayList<Long>();
			for(long hwnd : visibleTopLevelWindows){				
				boolean owned = Windows.GetWindow(hwnd, Windows.GW_OWNER) != 0;
				boolean include = Windows.GetWindowProcessId(hwnd) == uiaRoot.pid;
				
				if(include){
					if(!owned)
						uiaDescend(uiaCacheWindowTree(hwnd), uiaRoot);
					else
						ownedWindows.add(hwnd);
				}
			}

			// if UIAutomation missed an owned window, we'll collect it here
			for(long hwnd : ownedWindows){				
				if(!uiaRoot.hwndMap.containsKey(hwnd))
					uiaDescend(uiaCacheWindowTree(hwnd), uiaRoot);					
			}

			// set z-indices for the windows
			int z = 0;
			for(long hwnd : visibleTopLevelWindows){
				long exStyle = Windows.GetWindowLong(hwnd, Windows.GWL_EXSTYLE);				
				if((exStyle & Windows.WS_EX_NOACTIVATE) != 0)
					System.out.println(hwnd  + "   " + Windows.GetWindowText(hwnd) + "   " + Windows.GetClassName(hwnd));

				UIAElement wnd = uiaRoot.hwndMap.get(hwnd);

				// if we didn't encounter the window yet, it will be a foreign window
				if(wnd == null){
					wnd = new UIAElement(uiaRoot);
					uiaRoot.children.add(wnd);
					wnd.ignore = true;
					wnd.hwnd = hwnd;
					long r[] = Windows.GetWindowRect(hwnd);
					if(r[2] - r[0] >= 0 && r[3] - r[1] >= 0)
						wnd.rect = Rect.fromCoordinates(r[0], r[1], r[2], r[3]);
					wnd.ctrlId = Windows.UIA_WindowControlTypeId;
					uiaRoot.hwndMap.put(hwnd, wnd);
				}				
				wnd.zindex = z++;

				if(wnd.ctrlId == Windows.UIA_MenuControlTypeId || wnd.ctrlId == Windows.UIA_WindowControlTypeId || wnd.parent == uiaRoot)
					wnd.isTopLevelContainer = true;
			}

			calculateZIndices(uiaRoot, 0);
			buildTLCMap(uiaRoot);
			markBlockedElements(uiaRoot);

			return uiaRoot;
		}

		/* lists all visible top level windows in ascending z-order (foreground window last) */
		Iterable<Long> visibleTopLevelWindows(){
			Deque<Long> ret = new ArrayDeque<Long>();
			long hwnd = Windows.GetWindow(Windows.GetDesktopWindow(), Windows.GW_CHILD);

			while(hwnd != 0){
				if(Windows.IsWindowVisible(hwnd)){
					
					long exStyle = Windows.GetWindowLong(hwnd, Windows.GWL_EXSTYLE);				
					if((exStyle & Windows.WS_EX_TRANSPARENT) == 0 && (exStyle & Windows.WS_EX_NOACTIVATE) == 0)
						ret.addFirst(hwnd);
				}
				hwnd = Windows.GetNextWindow(hwnd, Windows.GW_HWNDNEXT);
			}
			return ret;
		}

		/* fire up the cache request */
		long uiaCacheWindowTree(long hwnd){
			return Windows.IUIAutomation_ElementFromHandleBuildCache(pAutomation, hwnd, pCacheRequest);
		}

		void buildTLCMap(UIARootElement root){
			ElementMap.Builder builder = ElementMap.newBuilder();
			buildTLCMap(builder, root);
			root.tlc = builder.build();		
		}

		void buildTLCMap(ElementMap.Builder builder, UIAElement el){
			if(el.isTopLevelContainer)
				builder.addElement(el);
			for(int i = 0; i < el.children.size(); i++)
				buildTLCMap(builder, el.children.get(i));
		}

		void uiaDescend(long uiaPtr, UIAElement parent){
			if(uiaPtr == 0)
				return;

			UIAElement el = new UIAElement(parent);
			parent.children.add(el);

			el.ctrlId = Windows.IUIAutomationElement_get_ControlType(uiaPtr, true);			
			el.hwnd = Windows.IUIAutomationElement_get_NativeWindowHandle(uiaPtr, true);

			// bounding rectangle
			long r[] = Windows.IUIAutomationElement_get_BoundingRectangle(uiaPtr, true);
			if(r != null && r[2] - r[0] >= 0 && r[3] - r[1] >= 0)
				el.rect = Rect.fromCoordinates(r[0], r[1], r[2], r[3]);

			el.enabled = Windows.IUIAutomationElement_get_IsEnabled(uiaPtr, true);
			el.name = Windows.IUIAutomationElement_get_Name(uiaPtr, true);
			el.helpText = Windows.IUIAutomationElement_get_HelpText(uiaPtr, true); 
			el.automationId = Windows.IUIAutomationElement_get_AutomationId(uiaPtr, true); 
			el.className = Windows.IUIAutomationElement_get_ClassName(uiaPtr, true); 
			el.providerDesc = Windows.IUIAutomationElement_get_ProviderDescription(uiaPtr, true); 
			el.frameworkId = Windows.IUIAutomationElement_get_FrameworkId(uiaPtr, true); 
			el.orientation = Windows.IUIAutomationElement_get_Orientation(uiaPtr, true); 
			el.hasKeyboardFocus = Windows.IUIAutomationElement_get_HasKeyboardFocus(uiaPtr, true); 
			el.isKeyboardFocusable = Windows.IUIAutomationElement_get_IsKeyboardFocusable(uiaPtr, true);

			parent.root.hwndMap.put(el.hwnd, el);

			// get extra infos from windows
			if(el.ctrlId == Windows.UIA_WindowControlTypeId){
				long uiaWndPtr = Windows.IUIAutomationElement_GetPattern(uiaPtr, Windows.UIA_WindowPatternId, true);
				if(uiaWndPtr != 0){
					el.wndInteractionState = Windows.IUIAutomationWindowPattern_get_WindowInteractionState(uiaWndPtr, true);
					el.blocked = (el.wndInteractionState != Windows.WindowInteractionState_ReadyForUserInteraction);
					el.isWndTopMost = Windows.IUIAutomationWindowPattern_get_IsTopmost(uiaWndPtr, true);
					el.isModal = Windows.IUIAutomationWindowPattern_get_IsModal(uiaWndPtr, true);
					Windows.IUnknown_Release(uiaWndPtr);
				}
			}

			// descend children
			long uiaChildrenPtr = Windows.IUIAutomationElement_GetCachedChildren(uiaPtr);
			Windows.IUnknown_Release(uiaPtr);

			if(uiaChildrenPtr != 0){
				long count = Windows.IUIAutomationElementArray_get_Length(uiaChildrenPtr);

				if(count > 0){
					el.children = new ArrayList<UIAElement>((int)count);

					for(int i = 0; i < count; i++){
						long ptrChild = Windows.IUIAutomationElementArray_GetElement(uiaChildrenPtr, i);
						if(ptrChild != 0){
							uiaDescend(ptrChild, el);
						}
					}
				}
				Windows.IUnknown_Release(uiaChildrenPtr);
			}
		}


		private void markBlockedElements(UIAElement element){
			for(UIAElement c : element.children){
				if(element.blocked && !(c.ctrlId == Windows.UIA_WindowControlTypeId && c.blocked == false))
					c.blocked = true;
				markBlockedElements(c);
			}
		}

		private void calculateZIndices(UIAElement el, double wndZ){
			if(!el.isTopLevelContainer && el.parent != null)
				el.zindex = el.parent.zindex;

			for(int i = 0; i < el.children.size(); i++)
				calculateZIndices(el.children.get(i), wndZ);
		}

		private UIAState createWidgetTree(UIARootElement root){
			UIAState state = new UIAState(root);
			root.backRef = state;

			for(UIAElement childElement : root.children){
				if(!childElement.ignore)
					createWidgetTree(state, childElement);
			}
			return state;
		}

		private void createWidgetTree(UIAWidget parent, UIAElement element){
			UIAWidget w = parent.root().addChild(parent, element);
			element.backRef = w;
			for(UIAElement child : element.children)
				createWidgetTree(w, child);
		}
	}

	public UIAState apply(SUT system) throws StateBuildException {
		//default state
		UIARootElement uiaRoot = new UIARootElement();
		uiaRoot.isRunning = system.isRunning();

		long[] info = Windows.GetMonitorInfo(Windows.GetPrimaryMonitorHandle());
		uiaRoot.rect = Rect.fromCoordinates(info[1], info[2], info[3], info[4]);
		uiaRoot.timeStamp = System.currentTimeMillis();
		uiaRoot.hasStandardKeyboard = system.get(Tags.StandardKeyboard, null) != null;
		uiaRoot.hasStandardMouse = system.get(Tags.StandardMouse, null) != null;
		
		try {
			Future<UIAState> future = executor.submit(new StateFetcher(system));
			return future.get((long)(timeOut * 1000.0), TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			throw new StateBuildException(e);
		} catch (ExecutionException e) {
			throw new StateBuildException(e);
		} catch (TimeoutException e) {
			UIAState ret = new UIAState(uiaRoot);
			ret.set(Tags.Role, Roles.Process);
			ret.set(Tags.NotResponding, true);
			return ret;
		}
	}
}
