package eu.fittest.phplog.eventabstraction;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import eu.fittest.eventabstraction.Abstraction;
import eu.fittest.eventabstraction.Eventabstraction;
import eu.fittest.eventabstraction.Param;
import eu.fittest.eventabstraction.Path;
import eu.fittest.eventabstraction.Rewrites;
import eu.fittest.eventabstraction.Rule;

public class Abstractor
{
    
    private static final String ENCODING = "UTF8";
    private static final AbstractionFunction DEFAULT_ABSFUN = new AbstractionFunction("eu.fittest.phplog.abstraction.Ignore");
    private final List<RegexpMatcher> matchers;
    private final Map<String,Map<String,IAbstractionFunction>> abstractions; 
    private final Map<String,IAbstractionFunction> defaultAbstractions; 
    
    public Abstractor()
    {
        matchers = Collections.emptyList();
        abstractions = Collections.emptyMap();
        defaultAbstractions = Collections.emptyMap();
    }
    
    public Abstractor(List<RegexpMatcher> matchers, Map<String,IAbstractionFunction> defaultAbstractions, Map<String,Map<String,IAbstractionFunction>> abstractions)
    {
        this.matchers = matchers;
        this.defaultAbstractions = defaultAbstractions;
        this.abstractions = abstractions;
    }
    
    public Abstractor(Eventabstraction result)
    {
        matchers = makeMatchers(result.getRewrites());
        defaultAbstractions = new HashMap<String, IAbstractionFunction>();
        makeAbstractions(defaultAbstractions, result.getAbstractions().getAbstraction());
        abstractions = new HashMap<String, Map<String,IAbstractionFunction>>();
        makePathAbstractions(abstractions, result.getAbstractions().getPath());
    }

    public Event abstractEvent(String url, List<Parameter> get,List<Parameter> post, List<Parameter> cookie )
    {
        String simpleURL = rewrite(url);
        int index= simpleURL.indexOf('?');
        
        String thePath;
        if(index == -1)
        {
            thePath = simpleURL;
        }
        else
        {
            thePath = simpleURL.substring(0, index);
            String queryString = simpleURL.substring(index+1, simpleURL.length());
            get = new ArrayList<Parameter>(get);
            String[] params = queryString.split("&");
            for (String param : params)
            {
                String[] parts = param.split("=", 2);
                if(parts.length == 2)
                { 
                    try
                    {
                        get.add(new Parameter( URLDecoder.decode(parts[0],ENCODING), new VLiteral(URLDecoder.decode(parts[1], ENCODING))));
                    } catch (UnsupportedEncodingException e)
                    {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        
        Map<String,String> parameters = new HashMap<String, String>();
        abstractParameters(parameters, thePath, cookie);
        abstractParameters(parameters, thePath, post);
        abstractParameters(parameters, thePath, get);
        return new Event(thePath,parameters);
        
    }

    private void abstractParameters(Map<String, String> result, String path, List<Parameter> params)
    {
        Map<String, IAbstractionFunction> abs = abstractions.get(path);
        if(abs == null)
            abs = Collections.emptyMap();
        for (Parameter parameter : params)
        {
            IAbstractionFunction absFun = abs.get(parameter.name);
            if(absFun == null)
                absFun = defaultAbstractions.get(parameter.name);
            if(absFun == null)
                absFun = DEFAULT_ABSFUN;
            String abstractValue = abstractValue(absFun, parameter.value);
            if(abstractValue != null)
            {
              result.put(parameter.name, abstractValue);
            }
        }
    }

    private String abstractValue(IAbstractionFunction function, Value value)
    {
        return function.call(value);
    }

    private String rewrite(String url)
    {
        for (RegexpMatcher matcher : matchers)
        {
            String rewritten = matcher.rewrite(url);
            if(rewritten != null)
            {
                return rewritten;
            }
        }
        return url;
    }

    private static void makePathAbstractions(
            Map<String, Map<String, IAbstractionFunction>> table,
            List<Path> paths)
    {
        for (Path path : paths)
        {
            List<Abstraction> abstractions = path.getAbstraction();
            
            String key = path.getPath();
            Map<String, IAbstractionFunction> absFunTable = table.get(key);
            if(absFunTable == null)
            {
                absFunTable = new HashMap<String, IAbstractionFunction>();
                table.put(key, absFunTable);
            }
            makeAbstractions(absFunTable, abstractions);
        }
    }

    private static void makeAbstractions(
            Map<String, IAbstractionFunction> table,
            List<Abstraction> abstractions)
    {
        for (Abstraction abstraction : abstractions)
        {
            List<Param> prms = abstraction.getParam();
            String variable = abstraction.getVariable();
            String function = abstraction.getFunction();
            IAbstractionFunction absFun = new AbstractionFunction(function);
            List<FunctionParameter> params = new ArrayList<FunctionParameter>();
            for (Param param : prms)
            {
                params.add(new FunctionParameter(param.getName(), param.getValue()));
            }
            absFun.setParams(params);
            table.put(variable, absFun);
        }
    }

    private static List<RegexpMatcher> makeMatchers(Rewrites rewrites)
    {
        List<RegexpMatcher> matchers = new ArrayList<RegexpMatcher>();
        for (Rule rule : rewrites.getRule())
        {
            String regex = rule.getMatch();
            String replaceTemplate = rule.getReplace();
            matchers.add(new RegexpMatcher(Pattern.compile(regex), replaceTemplate));
        }
        return matchers;
    }
}
