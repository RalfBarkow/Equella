//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2013.07.10 at 02:09:22 PM EST 
//


package com.tle.web.lti.imsx;

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
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0}resultRecord"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "resultRecord"
})
@XmlRootElement(name = "replaceResultRequest")
public class ReplaceResultRequest {

    @XmlElement(required = true)
    protected ResultRecordType resultRecord;

    /**
     * Gets the value of the resultRecord property.
     * 
     * @return
     *     possible object is
     *     {@link ResultRecordType }
     *     
     */
    public ResultRecordType getResultRecord() {
        return resultRecord;
    }

    /**
     * Sets the value of the resultRecord property.
     * 
     * @param value
     *     allowed object is
     *     {@link ResultRecordType }
     *     
     */
    public void setResultRecord(ResultRecordType value) {
        this.resultRecord = value;
    }

}