/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
