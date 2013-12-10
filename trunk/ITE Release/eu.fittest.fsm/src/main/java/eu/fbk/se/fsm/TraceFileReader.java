package eu.fbk.se.fsm;

import java.io.IOException;
import java.io.Reader;

public interface TraceFileReader
{
    public void open(Reader reader) throws IOException;
    public void close() throws IOException;
    public String nextEvent() throws IOException;
}
