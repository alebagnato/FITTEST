package eu.fittest.phplog;

import java.io.File;

public class IPSessionIdentifier implements SessionIdentifier<String>
{

    @Override
    public String getIdFor(File file)
    {
        String[] parts = file.getName().split("-");
        return parts.length >= 1? parts[0] : null;
    }
}
