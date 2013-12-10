package eu.fittest.phplog.analyzer;

public class PathItem
{

    public final String tag;
    public final int index;
    public final String id;
    public final String name;
    
    public PathItem(String tag, int index, String id, String name)
    {
        this.tag = tag;
        this.index = index;
        this.id = id;
        this.name = name;
    }
    public PathItem(String tag, int index)
    {
        this(tag, index,null, null);
    }
}
