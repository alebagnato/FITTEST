package eu.fittest.phplog.analyzer;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import eu.fittest.phplog.eventabstraction.ConcreteEvent;

public class A extends Element implements InteractiveElement
{

    @Override
    public List<Action> match(ConcreteEvent concreteEvent)
    {
        Map<String, String> attrs = this.getAttributes();
        String href = attrs.get("href");
        // drop spaces around href (most browsers do this as well)
        if(href == null) href="";
        href = href.trim();
        
        // TODO: better match of URL + "GET" parameters to href-attribute, including hostname part
        
        if(href.endsWith(concreteEvent.url) && concreteEvent.post.size() == 0)
        {
            return Collections.<Action>singletonList(new ClickLink(this));
        }
        return null;
    }
}
