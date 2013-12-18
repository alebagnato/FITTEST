package eu.fittest.logging.trtransf.classAnalysis;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Provides methods to get the compilation units of a target eclipse project.
 * Most of them are now suspended, because in the new version we prefer to avoid
 * tie to eclipse projects.
 * 
 * @author Ales Sturala
 *
 *  SUSPENDED
 */
public class JdtProject {
	
	/* 
	 * Returns I-compilation-units of a given project or null if no project is found;
	 * Only packages with given prefix are returned.
	 * 
	 * SUSPENDED.
	 *
	public static List<ICompilationUnit> GetProjectIUnits(String projectName, String prefix) {
		// here keep found compilation units
		ArrayList<ICompilationUnit> iunits = new ArrayList<ICompilationUnit>();
		
		// get me workspace project
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IProject project = workspace.getRoot().getProject(projectName);
		
		try {
			IPackageFragment[] packages = JavaCore.create(project).getPackageFragments();
			for (IPackageFragment mypackage : packages) {
				if (mypackage.getKind() == IPackageFragmentRoot.K_SOURCE
						&& mypackage.getElementName().startsWith(prefix)) {
					// compilation unit does not exist
					if (mypackage.getCompilationUnits().length == 0) {
						//System.out.println("No compilation unit found in " + mypackage.getPath().toString());
					}
					// compilation unit does not exist
					//else if (mypackage.getCompilationUnits().length > 1) {
						//System.out.println("There are more than 1 compilation units in " + mypackage.getPath().toString());

					for (ICompilationUnit iunit : mypackage.getCompilationUnits())
						iunits.add(iunit);
				}
			}

			return iunits;
			
		} catch (JavaModelException e) {
			e.printStackTrace();
		}

		return null;
	}
	*/
	
	/* 
	 * Returns compilation unit of a given iunit.
	 */
	public static CompilationUnit getCompilationUnit(ICompilationUnit iunit) {
		ASTParser parser = ASTParser.newParser(AST.JLS3);
		parser.setResolveBindings(true);
		parser.setSource(iunit);
		
		// WP, adding this:
		// In order to parse 1.5 code, some compiler options need to be set to 1.5
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		Map options = JavaCore.getOptions();
		JavaCore.setComplianceOptions(JavaCore.VERSION_1_5, options);
		parser.setCompilerOptions(options);
		
		CompilationUnit cu = (CompilationUnit)parser.createAST(null);
		cu.recordModifications();

		return cu;
	}

	/* 
	 * Returns compilation unit of a given project or null if no project is found.
	 * WP: why is the package fixed to "executable" ??
	 *
	 * SUSPENDED
	 * 
	public static List<CompilationUnit> GetCompilationUnits(String projectName) {
		
		List<ICompilationUnit> iunits = JdtProject.GetProjectIUnits(projectName, "executable");
		
		List<CompilationUnit> cus = new ArrayList<CompilationUnit>();
		for (ICompilationUnit iunit : iunits) {
			CompilationUnit cu = JdtProject.GetCompilationUnit(iunit); 
			cus.add(cu);
		}
		
		return cus;
	}
	*/
	
	/* 
	 * Returns compilation unit of a given project and package or null if no project is found
	 *
	 * SUSPENDED
	public static List<CompilationUnit> GetCompilationUnits(String projectName, String packageName) {
		List<ICompilationUnit> iunits = JdtProject.GetProjectIUnits(projectName, "executable");
		
		// return only those compilation units that are inside specified package
		List<CompilationUnit> cus = new ArrayList<CompilationUnit>();
		for (ICompilationUnit iunit : iunits) {
			CompilationUnit cu = JdtProject.GetCompilationUnit(iunit); 
			
			// if compilation unit belongs to given project and package, return
			if (cu.getPackage().getName().getFullyQualifiedName().equals(packageName))
				cus.add(cu);
		}
		
		return cus;
	}
	*/
}
