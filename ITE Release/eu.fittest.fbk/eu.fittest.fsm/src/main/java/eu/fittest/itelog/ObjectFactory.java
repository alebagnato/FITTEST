//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.02.10 at 12:49:09 PM CET 
//


package eu.fittest.itelog;

import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the eu.fittest.itelog package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: eu.fittest.itelog
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Body }
     * 
     */
    public Body createBody() {
        return new Body();
    }

    /**
     * Create an instance of {@link EType }
     * 
     */
    public EType createEType() {
        return new EType();
    }

    /**
     * Create an instance of {@link VType }
     * 
     */
    public VType createVType() {
        return new VType();
    }

    /**
     * Create an instance of {@link OType }
     * 
     */
    public OType createOType() {
        return new OType();
    }

    /**
     * Create an instance of {@link OType.Fd }
     * 
     */
    public OType.Fd createOTypeFd() {
        return new OType.Fd();
    }

}
