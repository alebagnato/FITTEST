package eu.fittest.phplog;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SessionTraces<T>
{
    private final int timeout;

    private final SessionIdentifier<T> sessionIdentifier;
    
    public SessionTraces(SessionIdentifier<T> sessionIdentifier, int timeout)
    {
        this.sessionIdentifier = sessionIdentifier;
        this.timeout = timeout;
    }

    private static final Comparator<? super File> LOGSORTER = 
        new Comparator<File>()
        {

            @Override
            public int compare(File f1, File f2)
            {
                return Long.signum(getTime(f1)-getTime(f2));
            }
        };
        
    private static long getTime(File file)
    {
        String name = file.getName();
        if(name.endsWith(".xml"))
            name = name.substring(0, name.length()-4);
        String[] parts = name.split("-");
        
        return parts.length >= 3? Long.parseLong(parts[2]) : 0;
    }
        
    public List<List<File>> getSessionTracesFor(List<File> logs)
    {
        Collections.sort(logs,LOGSORTER);
        List<List<File>> sessions = new ArrayList<List<File>>();
        Map<T, List<File>> currentSessions = new HashMap<T, List<File>>();
        
        for (File log : logs)
        {
            T key = sessionIdentifier.getIdFor(log);
            List<File> current = currentSessions.get(key);
            if(current == null || hasTimedOut(current, getTime(log)))
            {
                current = new ArrayList<File>();
                currentSessions.put(key, current);
                sessions.add(current);
            }
            current.add(log);
        }
        return sessions;
    }

    private boolean hasTimedOut(List<File> trace, long time)
    {
        boolean result;
        if(timeout <= 0)
        {
            result = true;
        }
        else
        {
            if(trace.size() > 0)
            {
                result = time - getTime(trace.get(trace.size()-1)) > timeout;
            }
            else
            {
                result = true;
            }
        }
        return result;
    }
}
