package eu.fittest.phplog.analyzer;

import java.util.List;

import eu.fittest.phplog.eventabstraction.ConcreteEvent;
//TODO: implement class
public class ImgMap extends Element implements InteractiveElement
{
    @Override
    public void addChild(Element element)
    {
       if(element instanceof Area)
       {
           children.add(element);
       }
    }

    @Override
    public List<Action> match(ConcreteEvent concreteEvent)
    {
       throw new UnsupportedOperationException();
    }

}
