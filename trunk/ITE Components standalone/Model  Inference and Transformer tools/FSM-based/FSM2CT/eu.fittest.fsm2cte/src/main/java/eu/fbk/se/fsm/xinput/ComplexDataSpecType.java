//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2013.12.20 at 02:50:18 PM CET 
//


package eu.fbk.se.fsm.xinput;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.QName;


/**
 * <p>Java class for complexDataSpecType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="complexDataSpecType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;group ref="{http://www.fbk.eu/xinput}simpleDataSpecType"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{http://www.fbk.eu/xinput}commonAttGroup"/>
 *       &lt;attribute name="base" use="required" type="{http://www.w3.org/2001/XMLSchema}QName" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "complexDataSpecType", propOrder = {
    "facets"
})
public class ComplexDataSpecType {

    @XmlElementRefs({
        @XmlElementRef(name = "ame = "minExclusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlEleminExclusivelusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(namaxExclusivelusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(namaxLaxInclusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(namaxe = "totalDigits", namespace = "http://www.fbk.eu/xinput", type = TotalDigits.class),
        @XmlElementRef(nminLaxInclusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(namaxInclusive", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(name = "enumeration", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.class),
        @XmlElementRef(name mentRef(name = "pattern", namespace = "http://www.fbk.eu/xinput", type = Pattern.class),
        @XmlElementRef(name = "totalDigits", namespace = "http://www.fbk.eu/xinput", type = TotalDigits.class),
        @XmlElementRef(ne = "maxLength", namespace = "http://www.fbk.eu/xinput", type = JAXBElement.cla
    })
    protected List<Object> facets;
    @XmlAttribute(required = true)
    protected QName base;
    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;
    @XmlAttribute
    protected String name;

    /**
     * Gets the value of the facets property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the facets property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFacets().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link JAXBElement }{@code <}{@link BElement }{@code <}{@link NumFacet }{@code >}
     * {@link JAXBElement }{@code <}{@link Facet }{@code >}
     * {@JAXBElement }{@code <}{@link NumFacet }{@code > }
     * {@link JAXBElement }{@code <}{@link NumFacet }{@code >}
     * {@link JAXBElement }{@code <}{@lNumFacet }{@code >}
     * {@link JAXBElement }{@code <}{@link NoFixedFacet }{@code >}
     * {@link JAXBElement }{@code <}{@link Facet }{@code >}
     * {@link TotalDigits }
     * {@link Facet }{@code >}
     * {@link TotalDigits }
     * {@link JAXBElement }{@code <}{@link NoFixedFacet }{@code >}
     * 
     * 
     */
    public List<Object> getFacets() {
        if (facets == null) {
            facets = new ArrayList<Object>();
        }
        return this.facets;
    }

    /**
     * Gets the value of the base property.
     * 
     * @return
     *     possible object is
     *     {@link QName }
     *     
     */
    public QName getBase() {
        return base;
    }

    /**
     * Sets the value of the base property.
     * 
     * @param value
     *     allowed object is
     *     {@link QName }
     *     
     */
    public void setBase(QName value) {
        this.base = value;
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