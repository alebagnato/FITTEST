package eu.fittest.eclipse.component.junit.wizard;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.model.jobs.SessionType;
import eu.fittest.eclipse.model.jobs.TestCase;

public class JUnitPreparationTask {
	private List<TestCase> _testcases;
	private IServiceRegistry _registry;
	private String _pathToProfile;
	
	public JUnitPreparationTask(List<TestCase> testcases, IServiceRegistry registry, String pathToProfile){
		_testcases = testcases;
		_registry = registry;
		_pathToProfile = pathToProfile;
	}
	
	public SessionProperties prepare(IFolder session, SessionType type) throws FITTESTException{
		SessionProperties sessionProperties = new SessionProperties();
		List<IFile> testcases = new ArrayList<IFile>();
		for(TestCase tc : _testcases){
			if(tc.isSelected()){
				testcases.add(tc.getFile());
			}
		}
	
		try {
			TestCaseUtils.copyTestCases(session, testcases);
			sessionProperties = new SessionProperties();
			
			File tempFolder = new File(System.getProperty("java.io.tmpdir"),session.getName());
			if(!tempFolder.exists() && !tempFolder.mkdirs()){
				throw new FITTESTException("Can't create temp folder to compile test cases: "+tempFolder.getAbsolutePath());	
			}
			sessionProperties.setTempFolder(tempFolder);
			
			if(_pathToProfile != null && _pathToProfile.length()>0){
				File profileFile = new File(_pathToProfile);
				ICompressionService compressionService = _registry.findService(ICompressionService.class);
				File zipped = compressionService.zipAndGzip(profileFile);
				sessionProperties.setCRC(_registry.findService(ICompressionService.class).checksum(zipped));
				sessionProperties.setFirefoxProfile(zipped);

			}
			
			TestCaseUtils.compileTestCases(tempFolder,session, ResourceUtils.collectFiles(session.getRawLocation().toFile(), "java"));
			return sessionProperties;
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		} catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		} catch (CoreException e) {
			throw new FITTESTException(e.getMessage());
		}
	}
}
