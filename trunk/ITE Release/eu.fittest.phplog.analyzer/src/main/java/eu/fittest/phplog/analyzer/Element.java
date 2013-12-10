package eu.fittest.phplog.analyzer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Element
{
    private final List<PathItem> path;
    private final Map<String,String> attributes;
    protected final List<Element> children;
    public Element()
    { 
        this.path = new ArrayList<PathItem>();
        attributes = new HashMap<String, String>();
        children = new ArrayList<Element>();
    }
        
    public Map<String, String> getAttributes()
    {
        return attributes;
    }

    public void addChild(Element element)
    {
    }
    
    public List<Element> getChildren()
    {
        return Collections.unmodifiableList(children);
    }
    
    @Override
    public String toString()
    {
        String xpath = "";
        for (PathItem item : path)
        {
            
            String base = item.tag + "[" +item.index+"]";
            if(item.id != null || item.name != null)
            {
                base += "[";
                String sep = "";
                if(item.id != null)
                {
                    base += sep + "@id='"+item.id+"'";
                    sep = " and ";
                }
                if(item.name != null)
                {
                    base += sep + "@name='"+item.name+"'";
                    sep = " and ";     
                }
                base += "]";
            }
            if(item.id != null)
            {
                xpath = "//" + base;
            }
            else
            {
                xpath += "/" + base;
            }
        }
        return this.getClass().getSimpleName() + xpath + attributes + children;
    }

    public void setPath(List<PathItem> path)
    {
        this.path.clear();
        this.path.addAll(path);
    }
    
    public List<PathItem> getPath()
    {
        return path;
    }
}
