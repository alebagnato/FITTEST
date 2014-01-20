//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-833 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2013.12.20 at 01:46:02 PM CET 
//


package eu.fbk.se.fsm.cte;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlRootElement;
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
 *       &lt;sequence>
 *         &lt;element name="Tree">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;choice maxOccurs="unbounded" minOccurs="0">
 *                     &lt;element ref="{}Composition"/>
 *                     &lt;element ref="{}Classification"/>
 *                     &lt;element ref="{}Class"/>
 *                   &lt;/choice>
 *                   &lt;group ref="{}tagGroup" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *                 &lt;attGroup ref="{}nameIDattr"/>
 *                 &lt;attribute name="root" type="{}cteIDREF" />
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element ref="{}TestGroup"/>
 *         &lt;group ref="{}tagGroup" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}nameIDattr"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "tree",
    "testGroup",
    "tagGroup"
})
@XmlRootElement(name = "CteObject")
public class CteObject {

    @XmlElement(name = "Tree", required = true)
    protected CteObject.Tree tree;
    @XmlElement(name = "TestGroup", required = true)
    protected TestGroup testGroup;
    @XmlElement(name = "Tag")
    protected List<eu.fbk.se.fsm.cte.Mark.Tag> tagGroup;
    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    protected String id;
    @XmlAttribute
    protected String name;

    /**
     * Gets the value of the tree property.
     * 
     * @return
     *     possible object is
     *     {@link CteObject.Tree }
     *     
     */
    public CteObject.Tree getTree() {
        return tree;
    }

    /**
     * Sets the value of the tree property.
     * 
     * @param value
     *     allowed object is
     *     {@link CteObject.Tree }
     *     
     */
    public void setTree(CteObject.Tree value) {
        this.tree = value;
    }

    /**
     * Gets the value of the testGroup property.
     * 
     * @return
     *     possible object is
     *     {@link TestGroup }
     *     
     */
    public TestGroup getTestGroup() {
        return testGroup;
    }

    /**
     * Sets the value of the testGroup property.
     * 
     * @param value
     *     allowed object is
     *     {@link TestGroup }
     *     
     */
    public void setTestGroup(TestGroup value) {
        this.testGroup = value;
    }

    /**
     * Gets the value of the tagGroup property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tagGroup property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTagGroup().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link eu.fbk.se.fsm.cte.Mark.Tag }
     * 
     * 
     */
    public List<eu.fbk.se.fsm.cte.Mark.Tag> getTagGroup() {
        if (tagGroup == null) {
            tagGroup = new ArrayList<eu.fbk.se.fsm.cte.Mark.Tag>();
        }
        return this.tagGroup;
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


    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence>
     *         &lt;choice maxOccurs="unbounded" minOccurs="0">
     *           &lt;element ref="{}Composition"/>
     *           &lt;element ref="{}Classification"/>
     *           &lt;element ref="{}Class"/>
     *         &lt;/choice>
     *         &lt;group ref="{}tagGroup" maxOccurs="unbounded" minOccurs="0"/>
     *       &lt;/sequence>
     *       &lt;attGroup ref="{}nameIDattr"/>
     *       &lt;attribute name="root" type="{}cteIDREF" />
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
        "compositionOrClassificationOrClazz",
        "tagGroup"
    })
    public static class Tree {

        @XmlElements({
            @XmlElement(name = "Classification", type = Classification.class),
            @XmlElement(name = "Class", type = Class.class),
            @XmlElement(name = "Class", type = Class.class),
          
        })
        protected List<Object> compositionOrClassificationOrClazz;
        @XmlElement(name = "Tag")
        protected List<eu.fbk.se.fsm.cte.Mark.Tag> tagGroup;
        @XmlAttribute
        @XmlIDREF
        protected Object root;
        @XmlAttribute(required = true)
        @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
        @XmlID
        protected String id;
        @XmlAttribute
        protected String name;

        /**
         * Gets the value of the compositionOrClassificationOrClazz property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the compositionOrClassificationOrClazz property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getCompositionOrClassificationOrClazz().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link Classification }
         * {@link Class }
         * {@link Composition }
         * 
         * 
         */
        public List<Object> getCompositionOrClassificationOrClazz() {
            if (compositionOrClassificationOrClazz == null) {
                compositionOrClassificationOrClazz = new ArrayList<Object>();
            }
            return this.compositionOrClassificationOrClazz;
        }

        /**
         * Gets the value of the tagGroup property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the tagGroup property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getTagGroup().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link eu.fbk.se.fsm.cte.Mark.Tag }
         * 
         * 
         */
        public List<eu.fbk.se.fsm.cte.Mark.Tag> getTagGroup() {
            if (tagGroup == null) {
                tagGroup = new ArrayList<eu.fbk.se.fsm.cte.Mark.Tag>();
            }
            return this.tagGroup;
        }

        /**
         * Gets the value of the root property.
         * 
         * @return
         *     possible object is
         *     {@link Object }
         *     
         */
        public Object getRoot() {
            return root;
        }

        /**
         * Sets the value of the root property.
         * 
         * @param value
         *     allowed object is
         *     {@link Object }
         *     
         */
        public void setRoot(Object value) {
            this.root = value;
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

}
