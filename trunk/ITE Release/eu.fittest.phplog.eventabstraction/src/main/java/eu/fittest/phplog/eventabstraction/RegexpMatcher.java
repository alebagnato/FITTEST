package eu.fittest.phplog.eventabstraction;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexpMatcher
{

    private final Pattern pattern;
    private final String replaceTemplate;
    
    public RegexpMatcher(Pattern pattern, String replaceTemplate)
    {
        this.pattern = pattern;
        this.replaceTemplate = replaceTemplate;
    }
    
    public String rewrite(String string)
    {
        Matcher matcher = pattern.matcher(string);       
        if(matcher.matches())
        {
            return matcher.replaceFirst(replaceTemplate);
        }        
        return null;
    }

}
