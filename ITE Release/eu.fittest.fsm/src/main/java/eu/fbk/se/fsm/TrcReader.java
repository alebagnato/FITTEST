package eu.fbk.se.fsm;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TrcReader implements TraceFileReader
{
    private BufferedReader reader;
    private final Pattern pattern;

    public TrcReader()
    {
        pattern = Pattern.compile("\\s*(.+)\\s*.*"); //ale
    }
    
    /*
     * returns the first trimmed non-empty line
     */
    @Override
    public String nextEvent() throws IOException
    {
        String line;
        String result = null;
        while((line = reader.readLine()) != null && result == null)
        {
            Matcher m = pattern.matcher(line);
            if (m.matches()) 
            {
                result =  m.group(1);
            }
        }
        return result;
    }

    @Override
    public void close() throws IOException
    {
        reader.close();
    }

    @Override
    public void open(Reader reader) throws IOException
    {
        this.reader = new BufferedReader(reader);        
    }
   
}