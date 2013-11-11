package eu.fittest;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import eu.fbk.se.fsm.TraceFileReader;
import eu.fittest.itelog.Body;
import eu.fittest.itelog.EType;
import eu.fittest.itelog.OType;
import eu.fittest.utils.XMLLogUtils;

public class FittestXMLReader implements TraceFileReader {
	
	private Body fittestLogBody = null;
	private List<String> events = null;
	private int currentIndex = -1;
	
	@Override
	public void open(Reader reader) throws IOException {
		fittestLogBody = XMLLogUtils.load(reader);
		if (fittestLogBody == null)
			throw new IOException("Failed to read log file!");
		
		// populate events
		List<EType> eList = fittestLogBody.getE();
		if (eList != null){
			events = new ArrayList<String>();
			
			for (EType event : eList){
				
				if (event.getO() != null){
					for (OType o : event.getO()){
						if (o.getTy() != null && o.getTy().endsWith("RecordEvent")){
							List<OType.Fd> fds = o.getFd();
							String eventId = "";
							for (OType.Fd fd : fds){
								
								if (fd.getN() != null && fd.getN().equals("targetID")){
									if (fd.getV() != null)
										eventId = fd.getV().getV();
									else 
										eventId = "targetIDEmpty";
									eventId = eventId + "_";
								}

								if (fd.getN() != null && fd.getN().equals("type")){
									if (fd.getV() != null)
										eventId = eventId + fd.getV().getV();
									else 
										eventId = eventId + "eventTypeEmpty";
								}
							}
							if (eventId.equals("")){
								eventId = "eventEmpty";
							}
							eventId = eventId.replaceAll("\"", "");
							events.add(eventId);
							break;
						}
					}
				}
				
			}
				
			currentIndex = 0;
		} else {
			throw new IOException("Empty log file!");
		}
	}

	@Override
	public void close() throws IOException {
		fittestLogBody = null;
		currentIndex = -1;
	}

	@Override
	public String nextEvent() throws IOException {
		if (events != null && currentIndex >= 0 && currentIndex < events.size()){
			return events.get(currentIndex++);
		}
		return null;
	}

}
