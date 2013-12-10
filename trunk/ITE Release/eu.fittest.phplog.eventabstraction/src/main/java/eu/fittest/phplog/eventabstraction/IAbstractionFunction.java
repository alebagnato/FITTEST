package eu.fittest.phplog.eventabstraction;

import java.util.List;

public interface IAbstractionFunction
{

    public void setParams(List<FunctionParameter> params);
    public String call(Value value);

}
