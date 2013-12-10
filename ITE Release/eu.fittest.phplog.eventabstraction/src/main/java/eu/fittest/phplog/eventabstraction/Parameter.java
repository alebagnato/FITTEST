package eu.fittest.phplog.eventabstraction;

public class Parameter
{
    public final String name;
    public final Value value;

    public Parameter(String name, Value value)
    {
        this.name = name;
        this.value = value;
    }
    
    @Override
    public String toString()
    {
        return name +"="+value;
    }
}
