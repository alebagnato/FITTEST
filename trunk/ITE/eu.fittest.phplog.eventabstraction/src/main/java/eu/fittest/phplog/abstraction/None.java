package eu.fittest.phplog.abstraction;

import java.util.List;

import eu.fittest.phplog.eventabstraction.FunctionParameter;
import eu.fittest.phplog.eventabstraction.IAbstractionFunction;
import eu.fittest.phplog.eventabstraction.Value;

public class None implements IAbstractionFunction
{

    @Override
    public void setParams(List<FunctionParameter>  params)
    {
    }

    @Override
    public String call(Value value)
    {
        return value.toString();
    }

}
