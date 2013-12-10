package eu.fittest.phplog;

import java.io.File;

public interface SessionIdentifier<T>
{
    public T getIdFor(File log);
}
