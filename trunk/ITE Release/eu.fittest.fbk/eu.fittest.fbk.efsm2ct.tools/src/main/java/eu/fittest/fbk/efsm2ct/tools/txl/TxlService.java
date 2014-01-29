package eu.fittest.fbk.efsm2ct.tools.txl;

import java.io.File;

import eu.fittest.fbk.efsm2ct.tools.CommandService;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;

public class TxlService extends CommandService {

	private String txlHome = "/home/tiella/programmi/txl10.6.linux64/";
	private String cmdName = "bin/txl";
	private File txlFile;
	
	// command line example: 
	// txl [txloptions] [-o outputfile] inputfile [txlfile] [- progoptions]
	
	/*
	 * Command options:
  -q               Quiet - turn off all information messages
  -v               Verbose - more detail in information messages
  -c               Compile program to TXL byte code file 'txlfile.CTxl'
  -l               Load and run TXL byte code file 'txlfile.CTxl'
  -d <symbol>      Define preprocessor symbol <symbol>
  -i <dir>         Add <dir> to the TXL include file search path
  -comment         Parse comments in input (default ignored)
  -char            Parse white space and newlines in input (default ignored)
  -newline         Parse newlines in input (default ignored)
  -multiline       Allow multiline tokens (default yes)
  -token           Ignore white space in input (separates input only - default)
  -id '<chars>'    Add each of <chars> to the identifier character set
  -sp '<chars>'    Add each of <chars> to the white space character set
  -esc '<char>'    Use <char> as the literal string escape character
  -upper           Shift input to upper case (except inside literals)
  -lower           Shift input to lower case (except inside literals)
  -case            Ignore case in input (but do not change it)
  -txl             Use TXL input lexical conventions
  -attr            Show attributes in output source (default invisible)
  -raw             Output source in raw (unspaced) form
  -w <width>       Wrap output source lines at <width> characters (default 128)
  -in <width>      Use output indent width of <width> characters (default 4)
  -tabnl           [TAB_nn] may force line wrap (default no)
  -xml             Output as XML parse tree
  -s <size>        Expand TXL tree space memory to <size> Mb (default 32)
  -analyze         Analyze grammar and rule set for ambiguities (slow)
  -u               Show TXL tree space memory usage statistics
  -o <file>        Write output to <file> (default standard output)
  -no<option>      Turn off <option> (e.g., -noraw)
  - <progoptions>  Pass <progoptions> to TXL program in global variable 'argv'

Debugging options, output to standard error stream:
  -Dscan           Show scanner input tokens
  -Dparse          Show parser input parse tree
  -Dresult         Show transformer final result parse tree
  -Dgrammar        Show grammar as a parse tree schema
  -Dpattern        Show pattern and replacement parse tree schemas
  -Drules          Show names of rules as they are applied
  -Dapply          Show trace of transformations by rules

	 */
	
	public TxlService(File txlFile) {
	
		this.txlFile = txlFile;
		
	}
	

	public void init(File inputFile, File outputFile) {
		
		super.init();
		
		addCmd(new File(this.txlHome, this.cmdName).getAbsolutePath());
		addCmd("-o");
		addCmd(outputFile.getAbsolutePath());
		addCmd(inputFile.getAbsolutePath());
		addCmd(txlFile.getAbsolutePath());
			
	}
	

	public File getTxlHomePath() {
		return new File(txlHome);
	}

	public void setTxlHomePath(File dirPath) {
		this.txlHome = dirPath.getAbsolutePath();
	}
	
	
}
