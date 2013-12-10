package eu.fittest.component.junit.gui;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

import eu.fittest.component.junit.IJunitDescriptionView;

public class SeleniumLogHandler extends Handler {
	private IJunitDescriptionView _view;
	
	public SeleniumLogHandler(IJunitDescriptionView view){
		_view = view;
	}
	
	@Override
	public void close() throws SecurityException {
		// TODO Auto-generated method stub

	}

	@Override
	public void flush() {
		// TODO Auto-generated method stub

	}

	@Override
	public void publish(LogRecord record) {
		_view.appendText(record.getMessage());
	}

}
