package eu.fittest.phplog.analyzer;

import java.util.List;

import eu.fittest.phplog.eventabstraction.ConcreteEvent;

public interface InteractiveElement
{
    public List<Action> match(ConcreteEvent concreteEvent);
}
