package eu.fittest.eclipse.component.junit.wizard;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IViewPart;

import eu.fittest.common.core.xml.Initialize;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.eclipse.component.IFITTESTComponentWizardPage;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.CenterImageLabelProvider;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.model.jobs.TestCase;

public class TestCaseSelectionPage extends WizardPage implements IFITTESTComponentWizardPage {
	private static final String PAGE_NAME = "New Replaying Session";

	
	private TableViewer _testcases;
	private IFolder _testsuites;

	
	@SuppressWarnings("unchecked")
	public List<TestCase> getTestCases(){
		return (List<TestCase>) _testcases.getInput();
	}
	
	public TestCaseSelectionPage() {
		super(PAGE_NAME);
	}

	private void findTestSuites(){
		IViewPart view = Viewer.getView(IFITTESTFolderConstants.NAVIGATOR);
		if(view!=null){
//			ISelectionService selectionService = view.getSite().getWorkbenchWindow().getSelectionService();
//			if(selectionService.getSelection() instanceof IStructuredSelection){
//				IStructuredSelection selection = (IStructuredSelection) selectionService.getSelection();
//				if(selection.getFirstElement()!=null){
//					IFolder selectedFolder = (IFolder)selection.getFirstElement();
			IProject selectedProject = Activator.getDefault().getActiveProject();
			if (selectedProject != null){
				_testsuites = selectedProject.getFolder(IFITTESTFolderConstants.TEST_SUITES);
			}
//				}
//			}
		}
	}
	
	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		composite.setLayout(gridLayout);

		GridData layoutData = new GridData();

		Group hostsGroup = new Group(composite, SWT.SHADOW_NONE);
		layoutData.horizontalSpan = 2;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.verticalAlignment = GridData.FILL;
		layoutData.grabExcessVerticalSpace = true;
		hostsGroup.setLayoutData(layoutData);

		gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		hostsGroup.setLayout(gridLayout);

		hostsGroup.setText("Test cases");


		_testcases = new TableViewer(hostsGroup, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		createColumns();
		_testcases.getTable().setLinesVisible (true);
		_testcases.getTable().setHeaderVisible (true);

		_testcases.setContentProvider(new ArrayContentProvider());

		try {
			fillTestCases();
		} catch (CoreException e1) {
			e1.printStackTrace();
		}

		layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
		layoutData.horizontalSpan = 2;
		layoutData.heightHint = 200;
		_testcases.getControl().setLayoutData(layoutData);

		// by urueda (allow mark/unmark of multiple checkboxes at once)
		_testcases.addDoubleClickListener(new IDoubleClickListener() {

			@Override
			public void doubleClick(DoubleClickEvent arg0) {
				IStructuredSelection sel = (IStructuredSelection) _testcases.getSelection();
				if (sel.isEmpty()) return;
				TestCase t;
				Object[] selection = sel.toArray().clone();
				for (Object obj : selection) {
					t = (TestCase) obj;
					t.setSelected(!t.isSelected());
					System.out.println(t.getFile() + " Changed: " + t.isSelected());
				}				
				_testcases.setSelection(null);
			}

		});	// end by urueda

		dialogChanged();
		setControl(composite);
	}	
	
	private void fillTestCases() throws CoreException {	
		findTestSuites();
		_testcases.setInput(findTestCases(_testsuites));
	}
	
	private List<TestCase> findTestCases(IFolder folder) throws CoreException{
		List<TestCase> lists = new ArrayList<TestCase>();
		if (folder == null)
			return lists;
		for(IResource r: folder.members()){
			if(r.getType()==IResource.FILE && r.getFileExtension().equals("java")){
				TestCase t =new TestCase((IFile)r);
				t.addPropertyChangeListener("selected", new PropertyChangeListener() {
					
					@Override
					public void propertyChange(PropertyChangeEvent evt) {
						dialogChanged();
					}
				});
				lists.add(t);
			}
			else if(r.getType() == IResource.FOLDER){
				lists.addAll(findTestCases((IFolder)r));			}
		}
		return lists;
	}
	
	private void dialogChanged() {
		if(!oneTestCaseisSelected()){
			updateStatus("Please select at least one test case to run");
		}
		else {
			updateStatus(null);
		}
	}
	
	@SuppressWarnings("unchecked")
	private boolean oneTestCaseisSelected() {
		boolean selected = false;
		Iterator<TestCase> testcases = ((java.util.List<TestCase>) _testcases.getInput()).iterator();
		while(!selected && testcases.hasNext()){
			selected = testcases.next().isSelected();
		}
		return selected;
	}

	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	
	private void createColumns() {
		String[] titles = {"Name", "To run"};
		int[] bounds = { 250, 100};

		TableViewerColumn col = Viewer.createTableViewerColumn(_testcases, titles[0], bounds[0], 0);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				TestCase t = (TestCase) element;
				return t.getFile().getProjectRelativePath().makeRelativeTo(_testsuites.getProjectRelativePath()).toString();
			}
		});
	
		col = Viewer.createTableViewerColumn(_testcases, titles[1], bounds[1], 1);
		col.setLabelProvider(new CenterImageLabelProvider() {

			@Override
			public Image getImage(Object element) {
				if (((TestCase) element).isSelected()) {
					return Viewer.CHECKED_ICON;
				} else {
					return Viewer.UNCHECKED_ICON;
				}
			}			
		});
		col.setEditingSupport(new TestCaseEditingSupport(_testcases));
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public Collection<Parameter> getInitializationParameters() {
		return new Vector<Initialize.Parameter>();
	}
}
