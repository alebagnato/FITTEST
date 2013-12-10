package eu.fittest.phplog.analyzer;

import java.util.List;


public interface FormElement
{
    public SingleResult interpret();

    public List<Action> createActions(String string);
}
