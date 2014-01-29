package eu.fittest.phplog.eventabstraction;

import java.util.List;

public final class AbstractionFunction implements IAbstractionFunction
{   
    private final String name;
    private List<FunctionParameter> params;
    private IAbstractionFunction implementation;
    
    public AbstractionFunction(String name)
    {
        this.name = name;
    }

    @Override
    public String call(Value value)
    {
        if(implementation == null)
            initImplementation();
        return implementation.call(value);
    }

    private void initImplementation()
    {
        Class<?> implClass;
        try
        {
            implClass = Class.forName(name);
        } catch (ClassNotFoundException e1)
        {
            try
            {
                implClass = Class.forName("eu.fittest.phplog.abstraction."+name);
            } catch (ClassNotFoundException e)
            {
                throw new RuntimeException(e);
            }
        }
        try
        {
            Object obj = implClass.newInstance();
            if(obj instanceof IAbstractionFunction)
            {
                implementation = (IAbstractionFunction) obj;
                implementation.setParams(params);
            }
            else
            {
                throw new RuntimeException("Not an instance of IAbstractionFunction: "+name);
            }
        } 
        catch (InstantiationException e)
        {
            throw new RuntimeException(e);
        }
        catch (IllegalAccessException e)
        {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setParams(List<FunctionParameter> params)
    {
        this.params = params;
        if(implementation != null)
            implementation.setParams(params);
    }

}
