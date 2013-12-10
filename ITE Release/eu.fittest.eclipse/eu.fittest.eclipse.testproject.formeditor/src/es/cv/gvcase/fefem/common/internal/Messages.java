package es.cv.gvcase.fefem.common.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "es.cv.gvcase.fefem.common.internal.messages"; //$NON-NLS-1$
	public static String ChooseDialog_Message;
	public static String ChooseDialog_Title;
	public static String EMFContainedCollectionEditionComposite_CreateElement;
	public static String EMFContainedCollectionEditionComposite_Filter;
	public static String EMFContainedCollectionEditionComposite_RemoveElement;
	public static String EMFContainedCollectionEditionComposite_Up;
	public static String EMFContainedCollectionEditionComposite_Down;
	public static String EMFContainedHierarchicalCollectionEditionComposite_Collapse;
	public static String EMFContainedHierarchicalCollectionEditionComposite_Expand;
	public static String EMFContainedHierarchicalCollectionEditionComposite_Filter;
	public static String EMFContainedHierarchicalCollectionEditionComposite_NewChild;
	public static String EMFContainedHierarchicalCollectionEditionComposite_NewRoot;
	public static String EMFContainedHierarchicalCollectionEditionComposite_Remove;
	public static String EMFPropertyMultipleEReferenceComposite_AddReference;
	public static String EMFPropertyMultipleEReferenceComposite_RemoveReference;
	public static String SearchableTree_CaseSensitive;
	public static String SearchableTree_Search;
	public static String EMFPropertyMultipleEENumComposite_AddReference;
	public static String EMFPropertyMultipleEENumComposite_RemoveReference;
	public static String EMFPropertyHierarchyEReferenceComposite_WindowTreeTitle;
	public static String EMFPropertyHierarchyEReferenceComposite_WindowTreeOkButton;
	public static String EMFPropertyHierarchyEReferenceComposite_WindowTreeCancelButton;
	public static String FilteredLoadResourceDialog_ResourcePatterns;
	public static String FilteredLoadResourceDialog_ResourcePattern;
	public static String FilteredLoadResourceDialog_ResourcePatternActive;
	public static String FilteredLoadResourceDialog_ResourcePatternDescription;
	public static String FilteredLoadResourceDialog_LoadedResources;
	public static String FilteredLoadResourceDialog_ResourceLocation;
	public static String FEFEMWizardPreloadResourcePage_ResourcePatterns;
	public static String FEFEMWizardPreloadResourcePage_ResourcePattern;
	public static String FEFEMWizardPreloadResourcePage_ResourcePatternDescription;
	public static String FEFEMWizardPreloadResourcePage_ResourceURIColumn;
	public static String FEFEMWizardPreloadResourcePage_NoResourcesMessage;
	public static String FEFEMWizardPreloadResourcePage_ValidPageMessage;
	public static String FEFEMWizardPreloadResourcePage_AddWorkspace;
	public static String FEFEMWizardPreloadResourcePage_AddFilesystem;
	public static String FEFEMWizardPreloadResourcePage_Remove;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
