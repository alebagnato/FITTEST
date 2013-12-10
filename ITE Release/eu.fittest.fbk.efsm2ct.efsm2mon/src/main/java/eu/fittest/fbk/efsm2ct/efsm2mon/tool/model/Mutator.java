/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.model;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.Configuration;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.compiler.OnTheFlyJavaCompiler;

/**
 * 
 * @author tiella
 */
public class Mutator {

	private String name;
	private String alias;
	private List<String> types = new ArrayList<String>();
	private List<Transition> transitions = new ArrayList<Transition>();

	public String getAlias() {
		return alias;
	}

	public void setAlias(String alias) {
		this.alias = alias;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean addArgumentType(String e) {
		return types.add(e);
	}

	public boolean addTransition(Transition e) {
		e.setMutator(this);
		return transitions.add(e);
	}

	public boolean addType(String e) {
		return types.add(e);
	}

	public boolean addAllTypes(Collection<? extends String> c) {
		return types.addAll(c);
	}

	public List<String> getTypes() {
		return types;
	}

	public List<Transition> getTransitions() {
		return transitions;
	}

	public String getFormalArgs() {

		StringBuilder sb = new StringBuilder();

		int k = 1;

		for (String t : types) {
			sb.append(t).append(" ").append("x").append(k);
			if (k < types.size()) {
				sb.append(", ");
			}

			k++;
		}

		return sb.toString();

	}

	public String getActualArgs() {

		StringBuilder sb = new StringBuilder();

		int k = 1;

		for (String t : types) {
			sb.append("x").append(k);
			if (k < types.size()) {
				sb.append(", ");
			}

			k++;

		}

		return sb.toString();

	}
	
	public String getActualArgsWithToString() {

		StringBuilder sb = new StringBuilder();

		int k = 1;

		for (String t : types) {
			sb.append(", ").append("Integer.toString(").append("x").append(k).append(")");
			
			k++;

		}

		return sb.toString();

	}

	public String getActualArgsConcatenated() {

		StringBuilder sb = new StringBuilder();

		int k = 1;

		for (String t : types) {
			sb.append("x").append(k);
			if (k < types.size()) {
				sb.append("+\", \"+");
			}

			k++;

		}

		if (sb.length() == 0) {
			sb.append("\"\"");
		}

		return sb.toString();

	}

	public Set<State> getSources() {

		Set<State> states = new HashSet<State>();

		for (Transition t : transitions) {
			states.add(t.getSource());
		}

		return states;

	}

	public Object getEq() {
		OnTheFlyJavaCompiler compiler = new OnTheFlyJavaCompiler();

		String myPackageName = this.getClass().getPackage().getName();

		String capName = capitalize(name);
		String baseClassName = "Accessor" + capName;

		final String className = myPackageName + "." + baseClassName;

		String src = buildSourceCode(myPackageName, baseClassName);

		// System.out.println("source code ----\n"+src+"\n----\n");

		compiler.doCompilation(className, src, Configuration.getInstance().getCompilerDestDir());

		try {
			Class cls = this.getClass().getClassLoader().loadClass(className);

			Constructor constructor = cls.getConstructor();

			return constructor.newInstance();

		} catch (InstantiationException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IllegalAccessException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (IllegalArgumentException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (InvocationTargetException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (NoSuchMethodException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (SecurityException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		} catch (ClassNotFoundException ex) {
			Logger.getLogger(Mutator.class.getName()).log(Level.SEVERE, null, ex);
		}

		return null;
	}

	private String buildSourceCode(String packageName, String className) {

		StringBuilder sb = new StringBuilder();

		sb.append("package ").append(packageName).append("; ");

		sb.append("class ");
		sb.append(className);

		sb.append(" { public ");
		sb.append(className);
		sb.append("() {} ");

		// Set<String> mutNames = new HashSet<>();
		// for (Transition t : list) {
		// final String mutName = t.getMutator().getName();
		// mutNames.add(mutName);
		// }

		// for (String t : mutNames) {

		sb.append("public boolean get");
		sb.append(capitalize(name));
		sb.append("(){ return true; }");

		// }

		String allNames[] = { "add", "rem", "pay" };

		for (String s : allNames) {

			if (!name.equals(s)) {
				sb.append("public boolean get");
				sb.append(capitalize(s));
				sb.append("(){ return false; }");
			}

		}

		sb.append("}");

		return sb.toString();
	}

	public String capitalize(String s) {

		byte[] bytes = s.getBytes();

		if (bytes[0] >= 'a' && bytes[0] <= 'z') {
			bytes[0] += ('A' - 'a');
		}

		return new String(bytes);

	}
	
	// TODO
	
	public String getTargetObject() {
		
		int idx = name.lastIndexOf('_');
		
		return name.substring(0,idx);
		
	}
	
	public String getEvent() {
		
		int idx = name.lastIndexOf('_');
		
		return name.substring(idx+1);
	}
	
}
