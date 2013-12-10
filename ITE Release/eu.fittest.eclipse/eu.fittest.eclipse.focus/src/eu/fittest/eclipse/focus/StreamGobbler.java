package eu.fittest.eclipse.focus;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

class StreamGobbler implements Runnable{	
    private InputStream _is;
    private Level _level;
    
    StreamGobbler(InputStream is, Level level){
        _is = is;
        _level = level;
    }
    
    public void run()
    {
        try{
            InputStreamReader isr = new InputStreamReader(_is);
            BufferedReader br = new BufferedReader(isr);
            String line=null;
            while ( (line = br.readLine()) != null){
            	Logger.getAnonymousLogger().log(_level, line);             
            }
        } catch (IOException ioe){
        	Logger.getAnonymousLogger().log(Level.INFO, ioe.getMessage());
        }
    }
}
