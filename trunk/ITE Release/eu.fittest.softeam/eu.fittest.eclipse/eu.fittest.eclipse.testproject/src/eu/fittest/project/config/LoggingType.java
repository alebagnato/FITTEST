//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.01.29 at 02:10:55 PM CET 
//


package eu.fittest.project.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LoggingType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LoggingType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Instrumentation" type="{http://www.fittest.eu/TestProject}InstrumentationType"/>
 *         &lt;element name="LogTarget" type="{http://www.fittest.eu/TestProject}LogTargetType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LoggingType", propOrder = {
    "instrumentation",
    "logTarget"
})
public class LoggingType {

    @XmlElement(name = "Instrumentation", required = true)
    protected InstrumentationType instrumentation;
    @XmlElement(name = "LogTarget", required = true)
    protected LogTargetType logTarget;

    /**
     * Gets the value of the instrumentation property.
     * 
     * @return
     *     possible object is
     *     {@link InstrumentationType }
     *     
     */
    public InstrumentationType getInstrumentation() {
        return instrumentation;
    }

    /**
     * Sets the value of the instrumentation property.
     * 
     * @param value
     *     allowed object is
     *     {@link InstrumentationType }
     *     
     */
    public void setInstrumentation(InstrumentationType value) {
        this.instrumentation = value;
    }

    /**
     * Gets the value of the logTarget property.
     * 
     * @return
     *     possible object is
     *     {@link LogTargetType }
     *     
     */
    public LogTargetType getLogTarget() {
        return logTarget;
    }

    /**
     * Sets the value of the logTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogTargetType }
     *     
     */
    public void setLogTarget(LogTargetType value) {
        this.logTarget = value;
    }

}
