package flexstore.coverage.plugin;

/*
 * Copyright 2001-2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

import apparat.tools.coverage.CoverageObserver;

/**
 * 
 * @goal instrument
 * 
 * @phase process-classes
 */
public class CoverageMojo extends AbstractMojo
{

    /**
     * Location of the input file.
     * 
     * @parameter
     * @required
     */
    private File inputSWF;

    /**
     * Source paths .
     * 
     * @parameter
     * @required
     */
    private List<File> sources;

    /**
     * Location of instrumentation log .
     * 
     * @parameter
     * @required
     */
    private File instrumentionLog;

    public void execute() throws MojoExecutionException
    {
        
        try
        {
            Instrumenter instrumenter = new Instrumenter();
            PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter(instrumentionLog)));

            InstrumentationLogWriter observer = new InstrumentationLogWriter(writer);
            instrumenter.instrument(inputSWF, sources, observer);
            writer.close();

        } catch (IOException e)
        {
            throw new MojoExecutionException(e.getMessage(),e);
        }
    }

    final class InstrumentationLogWriter implements CoverageObserver
    {
        private PrintWriter writer; 
        public InstrumentationLogWriter(PrintWriter writer)
        {
            this.writer = writer;
        }

        public void instrument(String file, int line)
        {
            writer.print(file);
            writer.print('\t');
            writer.print(line);
            writer.println();
        }
    }
}
