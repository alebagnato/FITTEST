package eu.fittest.phplog.analyzer;

import java.util.Collections;
import java.util.List;


public class Select extends Element  implements FormElement
{
    @Override
    public void addChild(Element element)
    {
       if(element instanceof Option)
       {
           children.add(element);
       }
    }

    @Override
    public SingleResult interpret()
    {
        String name = getAttributes().get("name");

        boolean isOptional = "multiple".equals(getAttributes().get("multiple"));
        return new SingleResult(name, null, false, isOptional);
    }

    @Override
    public List<Action> createActions(String string)
    {
        Option option = null;
        for (Element child : getChildren())
        {
            if(child instanceof Option && string != null && string.equals(child.getAttributes().get("value")))
            {
                option = (Option) child;
            }
        }
        if(option != null)
            return Collections.<Action>singletonList(new SelectOption(this, option));
        else
            return null;
    }

}
