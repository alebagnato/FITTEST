package eu.fittest.phplog.analyzer;

public class SingleResult
{
    public final String name;
    public final String value;
    public final boolean isSubmit;
    public final boolean isOptional;
    public SingleResult(String name, String value, boolean isSubmit, boolean isOptional)
    {
        this.name = name;
        this.value = value;
        this.isSubmit = isSubmit;
        this.isOptional = isOptional;
    }
}
