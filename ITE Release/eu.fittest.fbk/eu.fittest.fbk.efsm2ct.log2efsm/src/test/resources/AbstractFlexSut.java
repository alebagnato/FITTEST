package eu.fittest.efsm2ct.sut;

import eu.fittest.sbtest.driver.FlexDriver;
import eu.fittest.sbtest.driver.StateDescription;

/**
 * This abstract class contains features common to drivers 
 * @author tiella
 */
public class AbstractFlexSut {
    
    protected FlexDriver flexDriver;
    protected StateDescription sd;
    
    public AbstractFlexSut() {
        flexDriver = new FlexDriver();
    }

    public void shutdown() {
        flexDriver.shutdown();
    }

    public void startup() throws Exception {
        flexDriver.start();
    }
    
}