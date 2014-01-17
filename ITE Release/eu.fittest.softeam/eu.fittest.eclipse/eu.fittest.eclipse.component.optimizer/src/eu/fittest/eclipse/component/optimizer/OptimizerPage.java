package eu.fittest.eclipse.component.optimizer;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.component.optimizer.IOptimizerProvidedServices;
import eu.fittest.component.optimizer.SearchAlgorithm;
import eu.fittest.eclipse.component.contest.Activator;
import eu.fittest.eclipse.component.contest.wizard.ConTestPage;


public class OptimizerPage extends ConTestPage{	
	private Button _optimizeParameters;
	private boolean _enabledOptimizeParameters = false;
	private SearchAlgorithm _searchAlgorithm = null;
	private Combo _searchAlgorithmCombo;
	private Long _seed = -1L;
	private Text _seedText;
	
	public boolean isOptimizerEnabled() {
		return _enabledOptimizeParameters;
	}

	public OptimizerPage() {
		setTitle("ConTest optimizer initialization parameters");
	}
	
	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		Composite composite = (Composite) getControl();
		
		/******************************************************************/
		GridData layoutData = new GridData();
		_optimizeParameters = new Button(composite, SWT.CHECK);
		_optimizeParameters.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_enabledOptimizeParameters = _optimizeParameters.getSelection();
				_searchAlgorithmCombo.setEnabled(_enabledOptimizeParameters);
				_seedText.setEnabled(_enabledOptimizeParameters);
			}
		});
		
		layoutData.horizontalAlignment = SWT.RIGHT;
		_optimizeParameters.setLayoutData(layoutData);
		
		layoutData = new GridData();
		Label label = new Label(composite, SWT.NONE);
		label.setText("Enable ConTest parameters optimization");
		layoutData.horizontalSpan=2;
		layoutData.horizontalAlignment = SWT.LEFT;
		label.setLayoutData(layoutData);
		
		/******************************************************************/
		Group contestOptimizerGroup = new Group(composite, SWT.SHADOW_NONE);
		
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		contestOptimizerGroup.setLayout(gridLayout);
		
		contestOptimizerGroup.setText("ConTest optimizer");
		
		layoutData = new GridData();
		layoutData.horizontalSpan = 2;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.verticalAlignment = GridData.CENTER;
		contestOptimizerGroup.setLayoutData(layoutData);
		
		///////Search Algorithm//////////////
		label = new Label(contestOptimizerGroup, SWT.NONE);
		label.setText("Search algorithm:");
		
		layoutData = new GridData();
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);
		
		_searchAlgorithmCombo = new Combo(contestOptimizerGroup, SWT.VERTICAL |
				   SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
		for(SearchAlgorithm value : SearchAlgorithm.values()){
			_searchAlgorithmCombo.add(value.name());
		}
		
		_searchAlgorithmCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_searchAlgorithm = SearchAlgorithm.valueOf(_searchAlgorithmCombo.getText());

			}
		});
		
		_searchAlgorithm = SearchAlgorithm.values()[0];
		_searchAlgorithmCombo.setText(_searchAlgorithm.name());
		
		layoutData = new GridData();
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.horizontalAlignment = GridData.FILL;
		_searchAlgorithmCombo.setLayoutData(layoutData);
		
		///////Seed //////////////
			label = new Label(contestOptimizerGroup, SWT.NONE);
			label.setText("Seed value:");
			
			layoutData = new GridData();
			layoutData.horizontalAlignment = SWT.BEGINNING;
			label.setLayoutData(layoutData);
			
			_seedText = new Text(contestOptimizerGroup,  SWT.SINGLE|SWT.BORDER);
			layoutData = new GridData();
			layoutData.grabExcessHorizontalSpace = true;
			layoutData.horizontalAlignment = GridData.FILL;
			_seedText.setLayoutData(layoutData);
			_seedText.setToolTipText("Seed value is optional, useful to replay the same optimization");
			_seedText.setText(Activator.getDefault().getPreferenceStore().getString("seedNumber"));
			_seedText.addModifyListener(new ModifyListener() {
				
				@Override
				public void modifyText(ModifyEvent event) {
					try{
						if(_seedText.getText().equals("")){
							_seed = -1L;
						}
						else{
							_seed = Long.parseLong(_seedText.getText());
						}
						Activator.getDefault().getPreferenceStore().setValue("seedNumber", _seedText.getText());
					}catch (NumberFormatException e) {
						//managed in dialogChanged()
					}
					finally{
						dialogChanged();
					}
				}
			});
			
			
			
		
		_enabled.setSelection(true);
		_enabled.notifyListeners(SWT.Selection, new Event());
		_optimizeParameters.setSelection(true);
		_optimizeParameters.notifyListeners(SWT.Selection, new Event());
		enableContent();
		dialogChanged();
		_enabled.setEnabled(false);
		_optimizeParameters.setEnabled(false);
	}

	protected void enableContent(){
		super.enableContent();
		if(_optimizeParameters!=null) _optimizeParameters.setEnabled(isEnabled());
		if(_searchAlgorithmCombo!=null) _searchAlgorithmCombo.setEnabled(_enabledOptimizeParameters && isEnabled());
	}
	
	protected String dialogChanged() {
		String message = super.dialogChanged();
		if(_enabledOptimizeParameters){
			if(!_seedText.getText().equals("")){
				try{
					Long.parseLong(_seedText.getText());
				}catch(NumberFormatException e){
					message = "Seed number must be a long value";
				}
			}
		}
		updateStatus(message);
		return message;
	}
	

	public Collection<Parameter> getInitializationParameters() {
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		
		Parameter p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
		p.setName("optimizer.enabled");
		p.setValue(Boolean.toString(_enabledOptimizeParameters));
		parameters.add(p);
		
		if(_enabledOptimizeParameters){
			p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
			p.setName(IOptimizerProvidedServices.SEARCH_ALGORITHM);
			p.setValue(_searchAlgorithm.name());
			parameters.add(p);
			
			p = FITTESTSingleton.getObjectFactory().createInitializeParameter();
			p.setName(IOptimizerProvidedServices.RANDOM_NUMBER_SEED);
			p.setValue(_seed.toString());
			parameters.add(p);
		}
		parameters.addAll(super.getInitializationParameters());
		return parameters;
	}
	

}
