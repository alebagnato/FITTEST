package eu.fittest.phplog.abstraction;

import java.util.List;

import eu.fittest.phplog.eventabstraction.FunctionParameter;
import eu.fittest.phplog.eventabstraction.IAbstractionFunction;
import eu.fittest.phplog.eventabstraction.Value;

public class ZeroNonZero implements IAbstractionFunction
{

    @Override
    public void setParams(List<FunctionParameter> params)
    {
    }

    @Override
    public String call(Value value)
    {
        try
        {
            double d = Double.parseDouble(value.toString());
            return ""+ (d!=0);
        }
        catch (NumberFormatException e) {
            return "invalid";
        }
    }

}
