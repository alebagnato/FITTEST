//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.01.15 at 01:51:07 PM CET 
//


package eu.fittest.common.core.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;extension base="{}Message">
 *       &lt;sequence>
 *         &lt;element name="resource" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="resourceSize" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="checksum" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="inflate" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "resource",
    "resourceSize",
    "checksum",
    "inflate"
})
@XmlRootElement(name = "Upload")
public class Upload
    extends Message
{

    @XmlElement(required = true)
    protected String resource;
    protected long resourceSize;
    @XmlElement(defaultValue = "0")
    protected long checksum;
    @XmlElement(defaultValue = "false")
    protected boolean inflate;

    /**
     * Gets the value of the resource property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getResource() {
        return resource;
    }

    /**
     * Sets the value of the resource property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setResource(String value) {
        this.resource = value;
    }

    /**
     * Gets the value of the resourceSize property.
     * 
     */
    public long getResourceSize() {
        return resourceSize;
    }

    /**
     * Sets the value of the resourceSize property.
     * 
     */
    public void setResourceSize(long value) {
        this.resourceSize = value;
    }

    /**
     * Gets the value of the checksum property.
     * 
     */
    public long getChecksum() {
        return checksum;
    }

    /**
     * Sets the value of the checksum property.
     * 
     */
    public void setChecksum(long value) {
        this.checksum = value;
    }

    /**
     * Gets the value of the inflate property.
     * 
     */
    public boolean isInflate() {
        return inflate;
    }

    /**
     * Sets the value of the inflate property.
     * 
     */
    public void setInflate(boolean value) {
        this.inflate = value;
    }

}
