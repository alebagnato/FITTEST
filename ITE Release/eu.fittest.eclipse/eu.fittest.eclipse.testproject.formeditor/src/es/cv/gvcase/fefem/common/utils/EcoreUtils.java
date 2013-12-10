package es.cv.gvcase.fefem.common.utils;

import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.edit.command.SetCommand;
import org.eclipse.emf.edit.domain.EditingDomain;

public class EcoreUtils {

	public static String dumpEMFModel(EditingDomain ed, EObject root){
		String s = "";
		s = s + "Loaded resources: \n";
		Iterator riter = ed.getResourceSet().getResources().iterator();
		while(riter.hasNext()){
			Resource r = (Resource) riter.next();
			s = s + " R: "+r.getURI().toPlatformString(false) + (" loaded = " + (r.isLoaded()?"yes":"no"))+"\n";
			
		}
		s = s + "\n";
		if(root != null){
			TreeIterator titer = root.eAllContents();
			while(titer.hasNext()){
				Object o = titer.next();
				if(o instanceof EObject){
					s = s + ((EObject) o).toString() + " : "+((EObject) o).eResource().getURI().toPlatformString(false)+ " - " + (((EObject) o).eResource().isLoaded()?"loaded":"not loaded")+"\n";
					EObject eo = (EObject) o;
					Iterator<EStructuralFeature> ifeatures = eo.eClass().getEAllStructuralFeatures().iterator();
					while(ifeatures.hasNext()){
						EStructuralFeature sf = ifeatures.next();
						s = s  + "  **> " + sf.getName() + " : "+sf.getEType().getName()+ " packageType = " +sf.getEType().getEPackage().getName()+ " isPRoxy = "+(sf.getEType().getEPackage().eIsProxy()?"yes":"no")+"\n";
					}
				}
			}
		}
		
		
		return s;
	
	}
	
	public static void unsetAllReferences(EditingDomain ed, EObject o){
		
		
			
		// Iterate over all referenced instances from/to the object
		List<EObject> objReferenced = o.eCrossReferences();
		for(int j=0;j<objReferenced.size();j++){
			
		 	// Iterate over all existing EReference of the object
			List<EReference> references = o.eClass().getEAllReferences();
			for(int z=0;z<references.size();z++){
				
				if(references.get(z).getEReferenceType().isSuperTypeOf(objReferenced.get(j).eClass())){
					
					ed.getCommandStack().execute(SetCommand.create(ed, o, references.get(z), SetCommand.UNSET_VALUE));
					break;
				}
			}
			
		}
	}
	
	public static void unsetAllReferences(EditingDomain ed, List<EObject> os){
		Iterator<EObject> iter = os.iterator();
		while(iter.hasNext())
			unsetAllReferences(ed, iter.next());
	}
}
