package eu.fittest.agent.sut.ui.headless;

import java.io.ByteArrayInputStream;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;

public class SUTAgentHeadlessUITest {
	private SUTAgentHeadlessUI _ui;
	
	@Before
	public void setUp(){
		 _ui = new SUTAgentHeadlessUI();
	}
	
	public static void main(String[] args) {
		new SUTAgentHeadlessUI().open();
	}
	
	@Test
	public void test1SUTAgentHeadlessUI(){
		System.setIn(new ByteArrayInputStream("1\n1\nY\nE\n".getBytes()));
		String description = _ui.getDescription();
		_ui.open();
		Assert.assertEquals(HUTEnvironment.PRODUCTION, _ui.getHUTEnvironment());
		Assert.assertEquals(HUTType.SERVER, _ui.getHUTType());
		Assert.assertEquals(description, _ui.getDescription());
	}
	
	@Test
	public void test2SUTAgentHeadlessUI(){
		String newDescription = "this is a Firefox host";
		System.setIn(new ByteArrayInputStream(("2\n2\nN\n"+newDescription+"\nE\n").getBytes()));
		_ui.open();
		Assert.assertEquals(HUTEnvironment.TEST, _ui.getHUTEnvironment());
		Assert.assertEquals(HUTType.CLIENT, _ui.getHUTType());
		Assert.assertEquals(newDescription, _ui.getDescription());
	}

	@Test
	public void test3SUTAgentHeadlessUI(){
		System.setIn(new ByteArrayInputStream("0\n3\n2\n0\n3\n2\nY\nE\n".getBytes()));
		String description = _ui.getDescription();
		_ui.open();
		Assert.assertEquals(HUTEnvironment.TEST, _ui.getHUTEnvironment());
		Assert.assertEquals(HUTType.CLIENT, _ui.getHUTType());
		Assert.assertEquals(description, _ui.getDescription());
	}
	
	@Test
	public void test4SUTAgentHeadlessUI(){
		String newDescription = "this is a Firefox host.";
		System.setIn(new ByteArrayInputStream(("abc\n3\n\n2\nfklsdflkd\ndqdjkqjk\n2\nn\n"+newDescription+"\nz\ne\n").getBytes()));
		_ui.open();
		Assert.assertEquals(HUTEnvironment.TEST, _ui.getHUTEnvironment());
		Assert.assertEquals(HUTType.CLIENT, _ui.getHUTType());
		Assert.assertEquals(newDescription, _ui.getDescription());
	}
}
