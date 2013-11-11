package eu.fittest.phplog.eventabstraction;

import java.util.List;

public class ConcreteEvent
{

    public final String url;
    public final List<Parameter> get;
    public final List<Parameter> post;
    public final List<Parameter> cookie;
    public final boolean isRedirect;
    public final byte[] data;
    public final String mimeType;
/*
    public ConcreteEvent(String url, List<Parameter> get, List<Parameter> post, List<Parameter> cookie)
    {
        this(url, cookie, cookie, cookie, null, null);
    }*/
    public ConcreteEvent(String url, List<Parameter> get, List<Parameter> post
    		            , List<Parameter> cookie
    		            , boolean isRedirect
    		            , String mimeType, byte[] data)
    {
        this.url = url;
        this.get = get;
        this.post = post;
        this.cookie = cookie;
        this.isRedirect = isRedirect;
        this.mimeType = mimeType;
        this.data   = data;
    }
}
