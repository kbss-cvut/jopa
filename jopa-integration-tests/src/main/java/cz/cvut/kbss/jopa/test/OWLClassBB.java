/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_BB)
public class OWLClassBB implements HasUri {
    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_BB_INT_ATTRIBUTE)
    private int intAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_BOOLEAN_ATTRIBUTE)
    private boolean booleanAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_BYTE_ATTRIBUTE)
    private byte byteAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_SHORT_ATTRIBUTE)
    private short shortAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_LONG_ATTRIBUTE)
    private long longAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_FLOAT_ATTRIBUTE)
    private float floatAttribute;

    @OWLDataProperty(iri = Vocabulary.P_BB_DOUBLE_ATTRIBUTE)
    private double doubleAttribute;

    public OWLClassBB() {
    }

    public OWLClassBB(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public int getIntAttribute() {
        return intAttribute;
    }

    public void setIntAttribute(int intAttribute) {
        this.intAttribute = intAttribute;
    }

    public byte getByteAttribute() {
        return byteAttribute;
    }

    public void setByteAttribute(byte byteAttribute) {
        this.byteAttribute = byteAttribute;
    }

    public double getDoubleAttribute() {
        return doubleAttribute;
    }

    public void setDoubleAttribute(double doubleAttribute) {
        this.doubleAttribute = doubleAttribute;
    }

    public float getFloatAttribute() {
        return floatAttribute;
    }

    public void setFloatAttribute(float floatAttribute) {
        this.floatAttribute = floatAttribute;
    }

    public long getLongAttribute() {
        return longAttribute;
    }

    public void setLongAttribute(long longAttribute) {
        this.longAttribute = longAttribute;
    }

    public short getShortAttribute() {
        return shortAttribute;
    }

    public void setShortAttribute(short shortAttribute) {
        this.shortAttribute = shortAttribute;
    }

    public boolean getBooleanAttribute() {
        return booleanAttribute;
    }

    public void setBooleanAttribute(boolean booleanAttribute) {
        this.booleanAttribute = booleanAttribute;
    }

    @Override
    public String toString() {
        return "OWLClassBB{" +
                "uri=" + uri +
                ", intAttribute=" + intAttribute +
                ", booleanAttribute=" + booleanAttribute +
                ", byteAttribute=" + byteAttribute +
                ", shortAttribute=" + shortAttribute +
                ", longAttribute=" + longAttribute +
                ", floatAttribute=" + floatAttribute +
                ", doubleAttribute=" + doubleAttribute +
                '}';
    }
}
