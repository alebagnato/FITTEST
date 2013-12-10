package eu.fittest.phplog.analyzer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class HtmlContentHandler extends DefaultHandler
{
    Stack<Element> elements;
    List<InteractiveElement> result;
    Stack<Map<String, Integer>> siblingCount;
    Stack<PathItem> path;
    private final static Map<String, Class<? extends Element>> table;
    static {
        table = new HashMap<String, Class<? extends Element>>();
        table.put("form", Form.class);
        table.put("map", ImgMap.class);
        table.put("area", Area.class);
        table.put("a", A.class);
        table.put("option", Option.class);
        table.put("select", Select.class);
        table.put("input", Input.class);
        table.put("textarea", TextArea.class);
        table.put("button", Button.class);
    }
    
    public HtmlContentHandler()
    {
        elements = new Stack<Element>();
        result   = new ArrayList<InteractiveElement>();
        path     = new Stack<PathItem>();
        siblingCount = new Stack<Map<String,Integer>>();
    }
    
    
    @Override
    public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException
    {  
        
        String id = attributes.getValue("", "id");
        String name = attributes.getValue("", "name");

        path.push(new PathItem(localName,getSiblingCount(localName, siblingCount.peek())+1,id,name));    
        siblingCount.push(new HashMap<String, Integer>());
        
        Class<? extends Element> cls = table.get(localName);
        if(cls != null)
        {
            Element element;
            try
            {
                element = cls.newInstance();
                element.setPath(path);
            } catch (Exception e)
            {
                throw new RuntimeException(e);
            } 
            fillAttributes(attributes, element.getAttributes());
            elements.peek().addChild(element);
            elements.push(element);
        }
    }
    
    private void fillAttributes(Attributes attributes,
            Map<String, String> map)
    {
        for(int index = 0; index < attributes.getLength(); index++)
            map.put(attributes.getLocalName(index), attributes.getValue(index));    
    }

    @Override
    public void endElement(String uri, String localName, String qName)
            throws SAXException
    {
       if(table.containsKey(localName))
       {
           Element element = elements.pop();
           if(element instanceof InteractiveElement )
           {
               result.add((InteractiveElement)element);
           }
       }
       path.pop();
       siblingCount.pop();
       siblingCount.peek().put(localName, getSiblingCount(localName, siblingCount.peek())+1);
    }


    private int getSiblingCount(String localName, Map<String, Integer> p)
    {
        Integer val = p.get(localName);
        if(val == null)
            return 0;
        else
            return val.intValue();
    }

    @Override
    public void startDocument() throws SAXException
    {
        elements.clear();
        path.clear();
        siblingCount.clear();
        siblingCount.add(new HashMap<String, Integer>());
        result.clear();
        elements.push(new Element());
    }
    
    @Override
    public void endDocument() throws SAXException
    {
    }
    
    public List<InteractiveElement> getResult()
    {
        return result;
    }
}
