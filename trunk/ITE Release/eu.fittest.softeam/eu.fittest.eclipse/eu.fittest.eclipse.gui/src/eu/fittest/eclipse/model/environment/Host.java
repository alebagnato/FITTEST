package eu.fittest.eclipse.model.environment;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class Host implements Cloneable, PropertyChangeListener{
	private String _name;
	private String _ip;
	private String _description;
	private HUTEnvironment _environment;
	private HUTType _type;
	private boolean _isHUT;
	
	private Vector<String> lifeStates; // to store all the states of a host
	
	private StringBuffer activityLogBuffer = new StringBuffer();
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	
	private PropertyChangeSupport _support;
	
	public Host(String name, String ip, String description, HUTEnvironment environment, HUTType type, boolean isHUT) {
		_name = name;
		_ip = ip;
		_description = description;
		_environment = environment;
		_type = type;
		_isHUT = isHUT;
		
		_support = new PropertyChangeSupport(this);
		
		lifeStates = new Vector<String>(3);
		setCurrentLifeState(HostStates.NONE);
	}
	
	public Host(){
		this(null,null,null,null,null,false);
	}
	
	/**
	 * Add current life state 
	 * @param states
	 */
	public void setCurrentLifeState(String state){
		for (String s : HostStates.ALL_POSSIBLE_STATES){
			if (s.equals(state)){
				synchronized(lifeStates){
					lifeStates.add(0, state);
				}
				return;
			}
		}
		// not found, strage state
		synchronized (lifeStates){
			lifeStates.add(0, "UNKNOWN: " + state);
		}
	}
	
	/**
	 * Get current life state
	 * @param state
	 * @return
	 */
	public String getCurrentLifeState(){
		return lifeStates.get(0);
	}
	
	public Vector<String> getLifeStates(){
		return lifeStates;
	}
	
	public String getName() {
		return _name;
	}
	
	public void addPropertyChangeListener(String name, PropertyChangeListener l){
		_support.addPropertyChangeListener(name, l);
	}

	public void removePropertyChangeListener(PropertyChangeListener l){
		_support.removePropertyChangeListener(l);
	}
	
	@Override
	public int hashCode() {
		return _name.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (!(obj instanceof Host))	return false;
		Host other = (Host) obj;
		if (_name == null) {
			if (other._name != null)
				return false;
		} else if (!_name.equals(other._name))
			return false;
		return true;
	}

	public void setName(String name) {
		logActivity("Instantiated");
		_support.firePropertyChange("name", _name, _name = name);
	}
	public String getIp() {
		return _ip;
	}
	public void setIp(String ip) {
		_support.firePropertyChange("ip", _ip, _ip = ip);
	}
	public String getDescription() {
		return _description;
	}
	public void setDescription(String description) {
		_support.firePropertyChange("description", _description, _description = description);
	}
	public HUTEnvironment getEnvironment() {
		return _environment;
	}
	public void setEnvironment(HUTEnvironment environment) {
		_support.firePropertyChange("environment", _environment, _environment = environment);
	}
	public HUTType getType() {
		return _type;
	}
	public void setType(HUTType type) {
		_support.firePropertyChange("type", _type, _type = type);
	}
	public boolean isHUT() {
		return _isHUT;
	}
	public void setHUT(boolean isHUT) {
		_support.firePropertyChange("hUT", _isHUT, _isHUT = isHUT);
	}
	
	@Override
	public Host clone(){
		return new Host(_name, _ip, _description, _environment, _type, _isHUT);
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		
	}
	
	/**
	 * Get all the infor related to the host
	 */
	public String getInfo(){
		StringBuilder builder = new StringBuilder();
		builder.append("Agent name: ");
		builder.append(_name);
		builder.append("\n");
		
		builder.append("IP: ");
		builder.append(_ip);builder.append("\n");
		
		builder.append("Description: ");
		builder.append(_description);builder.append("\n");
		builder.append("\n");
		builder.append(_environment);builder.append("\n");
		builder.append(_type);builder.append("\n");
		
		return builder.toString();
	}
	
	/**
	 * @author cdnguyen
	 * Get the name of the agent
	 */
	public String toString(){
		if (_name != null)
			return _name;
		else return "No name";
	}
	
	/**
	 * Log an activity with respect to the host
	 * @author cdnguyen
	 * 
	 * @param activity
	 */
	public void logActivity(String activity){
		String currentDateTime = dateFormat.format(new Date());
		activityLogBuffer.append(currentDateTime + ":" + activity);
		activityLogBuffer.append("\n");
	}
	
	/**
	 * Get all activity logs
	 * @return
	 */
	public String getActivityLog(){
		return activityLogBuffer.toString();
	}

	/**
	 * Clear 256 chars from the buffer
	 */
	public void clearActivity() {
		int minLen = activityLogBuffer.length() - 256;
		if (minLen < 0) minLen = 0;
		activityLogBuffer.setLength(minLen);
	}
}
