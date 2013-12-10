package eu.fittest.phplog.abstraction;

import java.util.List;
import java.util.regex.Pattern;

import eu.fittest.phplog.eventabstraction.FunctionParameter;
import eu.fittest.phplog.eventabstraction.IAbstractionFunction;
import eu.fittest.phplog.eventabstraction.Value;

public class Regexp implements IAbstractionFunction
{
    public static final String INVALID = "invalid";
    List<FunctionParameter> regexps;

    @Override
    public void setParams(List<FunctionParameter> params)
    {
        regexps = params;
    }

    @Override
    public String call(Value value)
    {
        for (FunctionParameter entry : regexps)
        {
            if(Pattern.matches(entry.value, value.toString()))
                return entry.name;
        }
        return INVALID;
    }

}
