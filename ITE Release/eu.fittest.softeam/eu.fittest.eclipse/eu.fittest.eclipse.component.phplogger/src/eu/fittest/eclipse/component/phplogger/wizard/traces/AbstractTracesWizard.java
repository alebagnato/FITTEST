package eu.fittest.eclipse.component.phplogger.wizard.traces;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.Wizard;
import org.xml.sax.SAXException;

import eu.fittest.eventabstraction.Eventabstraction;
import eu.fittest.phplog.IPSessionIdentifier;
import eu.fittest.phplog.SessionTraces;
import eu.fittest.phplog.eventabstraction.LogFileAbstracter;
import eu.fittest.phplog.eventabstraction.LogFileConverter;
import eu.fittest.phplog.eventabstraction.ParseEventAbstraction;

public class AbstractTracesWizard extends Wizard {
	private AbstractTracesPage _page;
	private IFolder _outputFolder;
	private IFolder _selection;
	
	private static final FilenameFilter XMLFILESFILTER = new FilenameFilter()
	    {
	        
	        @Override
	        public boolean accept(File dir, String name)
	        {
	            return name.endsWith(".xml");
	        }
	    };
	public AbstractTracesWizard(IFolder selection, IFolder outputFolder) {
		_outputFolder = outputFolder;
		_selection = selection;
	}

    private static void convertLogFiles(File abstractionXML, File indir,
            File outdir) throws JAXBException, IOException, XMLStreamException, SAXException
    {        
        File[] logFiles = indir.listFiles(XMLFILESFILTER);
        Eventabstraction abstractionSpec = ParseEventAbstraction.parseEventAbstractionFile(abstractionXML);
        
        SessionTraces<String> sessionTraces = new SessionTraces<String>(new IPSessionIdentifier(), 900);
        List<List<File>> traces = sessionTraces.getSessionTracesFor(Arrays.asList(logFiles));

        LogFileConverter converter =  new LogFileConverter();
        LogFileAbstracter abstracter =  new LogFileAbstracter(abstractionSpec);
                
        File clientOutDir = new File(outdir, "client");
        if(!clientOutDir.exists() && !clientOutDir.mkdirs()) throw new IOException("can't create "+clientOutDir);
        File serverOutDir = new File(outdir, "server");
        if(!serverOutDir.exists() && !serverOutDir.mkdirs()) throw new IOException("can't create "+serverOutDir);

        File serverConcreteOutDir = new File(serverOutDir, "concrete");
        if(!serverConcreteOutDir.exists() && !serverConcreteOutDir.mkdirs()) throw new IOException("can't create "+serverConcreteOutDir);
        File serverAbstractOutDir = new File(serverOutDir, "abstract");
        if(!serverAbstractOutDir.exists() && !serverAbstractOutDir.mkdirs()) throw new IOException("can't create "+serverAbstractOutDir);

        File clientConcreteOutDir = new File(clientOutDir, "concrete");
        if(!clientConcreteOutDir.exists() && !clientConcreteOutDir.mkdirs()) throw new IOException("can't create "+clientConcreteOutDir);

        
        int counter = 0;
        for (List<File> trace : traces)
        {
        	String fileName = "log_" + counter + ".xml";
            File tmpfile = new File(serverConcreteOutDir, fileName) ;
            Writer tmp_out = new OutputStreamWriter(new FileOutputStream(tmpfile),"utf-8");
            converter.convert(trace, tmp_out);
            tmp_out.close();
            
            File outfile = new File(serverAbstractOutDir, fileName) ;
            Reader tmp_in = new InputStreamReader(new FileInputStream(tmpfile),"utf-8");
            Writer output = new OutputStreamWriter(new FileOutputStream(outfile),"utf-8");
            abstracter.convert(tmp_in, output);
            tmp_in.close();
            output.close();
            counter++;
        }       
    }
	@Override
	public boolean performFinish() {
		boolean result = false;
		try {
			File outputFolder = new File(_page.getPathToOutput());
			if(outputFolder.exists() || outputFolder.mkdirs()){			
				convertLogFiles(new File(_page.getPathToAbstractSpec()), new File(_page.getPathToInputFolder()), outputFolder );
				
				try {
					_outputFolder.refreshLocal(IFolder.DEPTH_INFINITE,null);
				} catch (CoreException e) {
					e.printStackTrace();
				}
				result = true;
			}
		} catch (JAXBException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (XMLStreamException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	@Override
	public void addPages() {
		super.addPages();
		
		_page = new AbstractTracesPage();
		String sessionName = _selection.getParent().getParent().getName().toLowerCase();
		_page.setPathToOutput(_outputFolder.getFolder(sessionName).getLocation().toOSString());
		_page.setPathToInputFolder(_selection.getFolder(sessionName).getLocation().toOSString());
		addPage(_page);
	}

}
