package eu.fittest.phplog.analyzer;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class Input extends Element implements FormElement
{     
         /*
        button   Defines a clickable button (mostly used with a JavaScript to activate a script)
        checkbox        Defines a checkbox
        file    Defines an input field and a "Browse..." button, for file uploads
        hidden  Defines a hidden input field
        image   Defines an image as a submit button
        password        Defines a password field. The characters in this field are masked
        radio   Defines a radio button
        reset   Defines a reset button. A reset button resets all form fields to their initial values
        submit  Defines a submit button. A submit button sends form data to a server
        text*/
       

    @Override
    public SingleResult interpret()
    {
        Map<String, String> attrs = getAttributes();
        String name = attrs.get("name");
        String value;
        String type = attrs.get("type");
        boolean isSubmit = "submit".equals(type) || "image".equals(type); //TODO: what about name.x and name.y ?

        if("hidden".equals(type) || "checkbox".equals(type) ||"radio".equals(type) || isSubmit)
            value = attrs.get("value");
        else 
            value = null;
        
        if("reset".equals(type) || "button".equals(type) )
            name = null;
        
        
        boolean isOptional = "checkbox".equals(type) ||"radio".equals(type) || isSubmit;
        return new SingleResult(name, value, isSubmit, isOptional);
    }

    /*
   checkbox        Defines a checkbox
    radio   Defines a radio button
   */
    
    @Override
    public List<Action> createActions(String string)
    {
        String value = getAttributes().get("value");
        String type = getAttributes().get("type");
        boolean isSubmit = "submit".equals(type) || "image".equals(type);
        if("text".equals(type) || "password".equals(type))
        {
            return Collections.<Action>singletonList(new Type(this, string));
        }
        else if(("radio".equals(type) || "checkbox".equals(type)) && string != null && string.equals(value))
        {
            return Collections.<Action>singletonList(new Check(this));
        }
        else if("hidden".equals(type) && string != null && string.equals(value) )
        {
            return Collections.emptyList();// no action
        }
        else if(isSubmit && (string == null || string.equals(value) ) ) 
        {
            return Collections.<Action>singletonList(new Submit(this));
        }
        return null;
    }

 

}
