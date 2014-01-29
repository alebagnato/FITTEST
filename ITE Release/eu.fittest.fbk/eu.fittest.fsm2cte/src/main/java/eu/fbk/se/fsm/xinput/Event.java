//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.01.29 at 02:09:01 PM CET 
//


package eu.fbk.se.fsm.xinput;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attGroup ref="{http://www.fbk.eu/xinput}commonAttGroup"/>
 *       &lt;attribute name="reachedById" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByName" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByTagName" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByLinkText" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByPartialLinkText" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByXPath" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByURL" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="reachedByCSS" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="webType" type="{http://www.fbk.eu/xinput}webElementType" />
 *       &lt;attribute name="targetEventToFire" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
@XmlRootElement(name = "event")
public class Event {

    @XmlAttribute
    protected String reachedById;
    @XmlAttribute
    protected String reachedByName;
    @XmlAttribute
    protected String reachedByTagName;
    @XmlAttribute
    protected String reachedByLinkText;
    @XmlAttribute
    protected String reachedByPartialLinkText;
    @XmlAttribute
    protected String reachedByXPath;
    @XmlAttribute
    protected String reachedByURL;
    @XmlAttribute
    protected String reachedByCSS;
    @XmlAttribute
    protected WebElementType webType;
    @XmlAttribute(required = true)
    protected String targetEventToFire;
    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute
    protected String name;

    /**
     * Gets the value of the reachedById property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedById() {
        return reachedById;
    }

    /**
     * Sets the value of the reachedById property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedById(String value) {
        this.reachedById = value;
    }

    /**
     * Gets the value of the reachedByName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByName() {
        return reachedByName;
    }

    /**
     * Sets the value of the reachedByName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByName(String value) {
        this.reachedByName = value;
    }

    /**
     * Gets the value of the reachedByTagName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByTagName() {
        return reachedByTagName;
    }

    /**
     * Sets the value of the reachedByTagName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByTagName(String value) {
        this.reachedByTagName = value;
    }

    /**
     * Gets the value of the reachedByLinkText property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByLinkText() {
        return reachedByLinkText;
    }

    /**
     * Sets the value of the reachedByLinkText property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByLinkText(String value) {
        this.reachedByLinkText = value;
    }

    /**
     * Gets the value of the reachedByPartialLinkText property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByPartialLinkText() {
        return reachedByPartialLinkText;
    }

    /**
     * Sets the value of the reachedByPartialLinkText property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByPartialLinkText(String value) {
        this.reachedByPartialLinkText = value;
    }

    /**
     * Gets the value of the reachedByXPath property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByXPath() {
        return reachedByXPath;
    }

    /**
     * Sets the value of the reachedByXPath property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByXPath(String value) {
        this.reachedByXPath = value;
    }

    /**
     * Gets the value of the reachedByURL property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByURL() {
        return reachedByURL;
    }

    /**
     * Sets the value of the reachedByURL property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByURL(String value) {
        this.reachedByURL = value;
    }

    /**
     * Gets the value of the reachedByCSS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReachedByCSS() {
        return reachedByCSS;
    }

    /**
     * Sets the value of the reachedByCSS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReachedByCSS(String value) {
        this.reachedByCSS = value;
    }

    /**
     * Gets the value of the webType property.
     * 
     * @return
     *     possible object is
     *     {@link WebElementType }
     *     
     */
    public WebElementType getWebType() {
        return webType;
    }

    /**
     * Sets the value of the webType property.
     * 
     * @param value
     *     allowed object is
     *     {@link WebElementType }
     *     
     */
    public void setWebType(WebElementType value) {
        this.webType = value;
    }

    /**
     * Gets the value of the targetEventToFire property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTargetEventToFire() {
        return targetEventToFire;
    }

    /**
     * Sets the value of the targetEventToFire property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTargetEventToFire(String value) {
        this.targetEventToFire = value;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setId(String value) {
        this.id = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

}
