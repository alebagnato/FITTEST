package eu.fittest.phplog.analyzer;

import java.util.Collections;
import java.util.List;


public class Button extends Element implements FormElement
{
    
    @Override
    public SingleResult interpret()
    {
        String name = getAttributes().get("name");
        String value = getAttributes().get("value");
        String type = getAttributes().get("type");
        if(!"submit".equals(type)) // ignore types: reset and button 
            name = null;
        boolean isSubmit = "submit".equals(type);
        boolean isOptional = true;
        return new SingleResult(name, value, isSubmit, isOptional);
    }

    @Override
    public List<Action> createActions(String string)
    {
        String value = getAttributes().get("value");
        String type = getAttributes().get("type");
        if((string == null || string.equals(value) ) && "submit".equals(type)) 
            return Collections.<Action>singletonList(new Submit(this));
        else
            return null;
    }

}
