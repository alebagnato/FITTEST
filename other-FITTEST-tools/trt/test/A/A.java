package A ;

import eu.fittest.tloglib.*;

// just some class that uses logging
public class A {

    public void f_logging() {  XLog.log("calling A.f1") ; }
    public void f_nonlogging() { }

    // non-logging, but in the class A2 will be overriden with
    // a version that logs.
    public void g() { }

}