package eu.fittest.phplog.analyzer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.fittest.phplog.eventabstraction.ConcreteEvent;
import eu.fittest.phplog.eventabstraction.Parameter;
import eu.fittest.phplog.eventabstraction.VArray;
import eu.fittest.phplog.eventabstraction.VLiteral;
import eu.fittest.phplog.eventabstraction.VObject;
import eu.fittest.phplog.eventabstraction.Value;

public class Form extends Element implements InteractiveElement
{

    @Override
    public void addChild(Element element)
    {
       if(element instanceof FormElement)
       {
           children.add(element);
       }
       else
       {
           for (Element child : element.getChildren())
           {
               addChild(child);
           }
       }
    }
   
    @Override
    public List<Action> match(ConcreteEvent concreteEvent)
    {
        Map<String, String> attrs = this.getAttributes();
        if("".equals(attrs.get("action")) || concreteEvent.url.equals(attrs.get("action")))
        {
            List<Parameter> params = "post".equalsIgnoreCase(attrs.get("method"))? concreteEvent.post : concreteEvent.get;
            Map<String,String> input = flattenParams(params);

            List<Result> results = interpret(0);
            outer:for (Result result : results)
            {
                if(result.containsSubmit)
                {
                    List<Action> actions = new ArrayList<Action>();
                    for (Entry<String, Map<String, Valuation>> entry : result.data.entrySet())
                    {
                        Map<String, Valuation> map = entry.getValue();
                        String varname = entry.getKey();
                        if(map.containsKey(null)) // simple value
                        {
                            Valuation valuation = map.get(null);
                            if(("".equals(varname) && isSubmit(valuation.element))
                                    ||
                             (input.containsKey(varname) && (valuation.value == null || input.get(varname).equals(valuation.value))))
                            {
                                List<Action> action = valuation.element.createActions(input.get(varname));
                                if(action != null) 
                                    actions.addAll(action);
                                else
                                    continue outer;
                            }
                            else
                            {
                               continue outer; 
                            }
                        }
                        else // array value
                        {
                            for (Entry<String, Valuation> entry2 : map.entrySet())
                            {
                                
                                String index = entry2.getKey();
                                String name = varname+"["+index+"]";
                                Valuation valuation = entry2.getValue();
                                
                                if(input.containsKey(name) && (valuation.value == null || input.get(name).equals(valuation.value)))
                                {
                                    List<Action> action = valuation.element.createActions(input.get(name));
                                    if(action != null) 
                                        actions.addAll(action);
                                    else
                                        continue outer;
                                }
                                else
                                {
                                    continue outer; 
                                }
                            }
                        }
                    }
                    postProcess(actions);
                    return actions;
                }
            }
        }
        return null;
    }

    private boolean isSubmit(FormElement element)
    {
        if(element instanceof Input)
        {
            Input input = (Input) element;
            return "submit".equalsIgnoreCase(input.getAttributes().get("type"));
        }
        return false;
    }

    /*
     * move submit action  to the end, and adjust checkboxes
     */
    private void postProcess(List<Action> actions)
    {
        List<Submit> submits = new ArrayList<Submit>();
        List<Check> checks   = new ArrayList<Check>();
        ;
        for (Iterator<Action> iter = actions.iterator(); iter.hasNext();)
        {
            Action action = iter.next();
            if(action instanceof Submit)
            {
                submits.add((Submit)action);
                iter.remove();
            }
            else if(action instanceof Check)
            {
                checks.add((Check)action);
                iter.remove();
            }
        }
        // switch off all checkboxes that are checked by default
        for (Element elem : getChildren())
        {
            if(elem instanceof Input && "checkbox".equals(elem.getAttributes().get("type")) && "selected".equals(elem.getAttributes().get("selected")) )
            {
                actions.add(new Check(elem,false));
            }
        }
        
        actions.addAll(checks);
        actions.addAll(submits);
    }

    private Map<String, String> flattenParams(List<Parameter> params)
    {
        Map<String, String> result = new HashMap<String, String>();
        for (Parameter parameter : params)
        {
            insertParam(result, parameter.name, parameter.value);
        }
        return result;
    }

    private void insertParam(Map<String, String> result, String name,
            Value value)
    {
        if(value instanceof VLiteral)
        {
            result.put(name,value.toString());
        }
        else 
        {
            Map<String, Value> data = Collections.emptyMap();
            if(value instanceof VObject)
            {
                data  = ((VObject)value).getFields();
            }
            else if(value instanceof VArray)
            {
                data = ((VArray)value).getEntries();
            }
            for (Entry<String, Value> entry : data.entrySet())
            {
                String varname = name+"[" + entry.getKey() +"]";
                insertParam(result, varname, entry.getValue());
            }
        }
    }

    private static final Pattern ARRAY_PATTERN = Pattern.compile("^(.*)\\[(.*)\\]$");
    private String[] toPHPVariable(String name)
    {
        
        Matcher matcher = ARRAY_PATTERN.matcher(name);
        String var, index;
        if(matcher.matches())
        {
            var   = matcher.group(1);
            index = matcher.group(2);
        }
        else
        {
            var = name;
            index = null;
        }
        StringBuilder string = new StringBuilder();
        for (char chr : var.toCharArray())
        {
            if(chr == ' ' || chr == '.' || chr == '[' || (chr >= 128 && chr <= 159))
                string.append('_');
            else
                string.append(chr);
        }
        return new String[]{string.toString(), index};
    }

    
    private static class Result
    {
        public final Map<String, Map<String,Valuation>> data;
        public final boolean containsSubmit;
        public Result()
        {
            data = new HashMap<String, Map<String,Valuation>>();
           containsSubmit = false;
        }
        public Result(Result result, boolean containsSubmit)
        {
            data = new HashMap<String, Map<String,Valuation>>();
            for (Entry<String, Map<String, Valuation>> entry : result.data.entrySet())
            {
                data.put(entry.getKey(), new HashMap<String, Valuation>(entry.getValue()));
            }
            this.containsSubmit = containsSubmit;
        }
        @Override
        public String toString()
        {
          
            return data.toString()+containsSubmit;
        }
    }

    List<Result> interpret(int index)
    {
        if(index >= children.size())
        {
           return Collections.<Result>singletonList(new Result());
        }
        else
        {
            Element current = children.get(index);
            List<Result> results = interpret(index+1);
            if(current instanceof FormElement && !"disabled".equals(current.getAttributes().get("disabled")))
            {
                FormElement felem = (FormElement) current;
                
                SingleResult felemResult = felem.interpret();
                               
                List<Result> newResults = new ArrayList<Result>(results.size());
                
                for (Result result : results)
                {

                    if(felemResult.isSubmit && result.containsSubmit)
                    {
                        newResults.add(result);
                        continue;
                    }
                    if(felemResult.isOptional || felemResult.name == null)
                        // name==null means disabled or otherwise unsuccessful input fields
                    {
                        newResults.add(result);
                    }
                    
                    if(felemResult.isSubmit || felemResult.name != null)
                    {
                        String[] phpvar = toPHPVariable(felemResult.name==null? "":felemResult.name);
                        String varname = phpvar[0];
                        String idx     = phpvar[1];
                        Result newResult  = new Result(result,felemResult.isSubmit || result.containsSubmit); ;
                        Map<String, Valuation> futurevalue = newResult.data.get(varname);
                        if(idx == null)
                        {
                            if(futurevalue == null)
                            {
                                Map<String, Valuation> map = new HashMap<String, Valuation>();
                                newResult.data.put(varname, map);
                                map.put(null, new Valuation(felem,felemResult.value));
                            }
                        }
                        else
                        {
                            if(futurevalue != null && futurevalue.containsKey(null))
                            {
                                //there is a later element in the form that is not an array, so skip;
                            }
                            else 
                            {
                                
                                if(futurevalue == null)
                                {
                                    futurevalue = new HashMap<String,Valuation>();
                                    newResult.data.put(varname,futurevalue);
                                }
                              
                                futurevalue.put(toIndex(idx,futurevalue), new Valuation(felem, felemResult.value));
                            }
                        }   
                      
                        newResults.add(newResult);
                    }
                }
                results = newResults;
            }
            return results;    
        }
    }

    private String toIndex(String idx, Map<String, ?> array)
    {
        if(idx.length() == 0)
        {
            int max = -1;
            for (String key : array.keySet())
            {
                if(key.matches("[0-9]+"))
                {
                    int tmp = Integer.parseInt(key);
                    if(tmp > max)
                        max = tmp;
                }
            }
            return ""+max+1;
        }
        else
        {
            return idx;
        }
    }
}
