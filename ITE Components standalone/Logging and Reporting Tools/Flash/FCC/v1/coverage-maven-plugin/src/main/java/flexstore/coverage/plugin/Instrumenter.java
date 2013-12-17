package flexstore.coverage.plugin;

import apparat.tools.coverage.CoverageConfiguration;
import apparat.tools.coverage.Coverage.CoverageTool;
import apparat.tools.coverage.CoverageObserver;
import java.io.File;

import scala.collection.immutable.List;
import scala.collection.mutable.ListBuffer;

public class Instrumenter
{
    public void instrument(File swf, java.util.List<File> sources, CoverageObserver observer)
    {

        CoverageTool c = new CoverageTool();
        c.configure(new CoverageConfigurationImpl(swf, swf, sources));
        
        if(observer != null)
            c.addObserver( observer );

        c.run();
    }

    final class CoverageConfigurationImpl implements CoverageConfiguration
    {
        private final ListBuffer<String> _sourcePath = new ListBuffer<String>();
        private final File _input;
        private final File _output;

        public CoverageConfigurationImpl(final File input, final File output,
                final java.util.List<File> sourcePath)
        {
            _input = input;
            _output = output;

            for (final File sourcePathElement : sourcePath)
            {
                _sourcePath.$plus$eq(sourcePathElement.getAbsolutePath());
            }
        }

        public File input()
        {
            return _input;
        }

        public File output()
        {
            return _output;
        }

        public List<String> sourcePath()
        {
            return _sourcePath.toList();
        }
    }

}