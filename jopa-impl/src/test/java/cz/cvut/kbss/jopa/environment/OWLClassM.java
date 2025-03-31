/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.PropertyInfo;
import cz.cvut.kbss.jopa.vocabulary.XSD;

import java.lang.reflect.Field;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Contains a generated string URI and data property attributes of primitive wrapper types - boolean, int, long, double.
 * Plus an enum attribute and a plural datatype property attribute.
 */
@OWLClass(iri = Vocabulary.c_OwlClassM)
public class OWLClassM {

    @Id(generated = true)
    private String key;

    @OWLDataProperty(iri = Vocabulary.p_m_booleanAttribute)
    private Boolean booleanAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_intAttribute)
    private Integer intAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_longAttribute)
    private Long longAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_doubleAttribute)
    private Double doubleAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_dateAttribute)
    private Date dateAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_characterAttribute)
    private Character characterAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_enumAttribute)
    private Severity enumAttribute;

    @Enumerated(EnumType.ORDINAL)
    @OWLDataProperty(iri = Vocabulary.p_m_ordinalEnumAttribute)
    private Severity ordinalEnumAttribute;

    @OWLDataProperty(iri = Vocabulary.p_m_IntegerSet)
    private Set<Integer> integerSet;

    @OWLDataProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
    private String lexicalForm;

    @OWLDataProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
    private String simpleLiteral;

    @OWLDataProperty(iri = Vocabulary.p_m_explicitDatatype, datatype = XSD.DURATION)
    private String explicitDatatype;

    @Convert(converter = ZoneOffsetConverter.class)
    @OWLDataProperty(iri = Vocabulary.p_m_withConverter, simpleLiteral = true)
    private ZoneOffset withConverter;

    @Enumerated(EnumType.OBJECT_ONE_OF)
    @OWLObjectProperty(iri = Vocabulary.p_m_objectOneOfEnumAttribute)
    private OneOfEnum objectOneOfEnumAttribute;

    public enum Severity {
        LOW, MEDIUM, HIGH
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public Boolean getBooleanAttribute() {
        return booleanAttribute;
    }

    public void setBooleanAttribute(Boolean booleanAttribute) {
        this.booleanAttribute = booleanAttribute;
    }

    public Integer getIntAttribute() {
        return intAttribute;
    }

    public void setIntAttribute(Integer intAttribute) {
        this.intAttribute = intAttribute;
    }

    public Long getLongAttribute() {
        return longAttribute;
    }

    public void setLongAttribute(Long longAttribute) {
        this.longAttribute = longAttribute;
    }

    public Double getDoubleAttribute() {
        return doubleAttribute;
    }

    public void setDoubleAttribute(Double doubleAttribute) {
        this.doubleAttribute = doubleAttribute;
    }

    public Date getDateAttribute() {
        return dateAttribute;
    }

    public void setDateAttribute(Date dateAttribute) {
        this.dateAttribute = dateAttribute;
    }

    public Character getCharacterAttribute() {
        return characterAttribute;
    }

    public void setCharacterAttribute(Character characterAttribute) {
        this.characterAttribute = characterAttribute;
    }

    public Severity getEnumAttribute() {
        return enumAttribute;
    }

    public void setEnumAttribute(Severity enumAttribute) {
        this.enumAttribute = enumAttribute;
    }

    public Set<Integer> getIntegerSet() {
        return integerSet;
    }

    public void setIntegerSet(Set<Integer> integerSet) {
        this.integerSet = integerSet;
    }

    public String getLexicalForm() {
        return lexicalForm;
    }

    public void setLexicalForm(String lexicalForm) {
        this.lexicalForm = lexicalForm;
    }

    public String getSimpleLiteral() {
        return simpleLiteral;
    }

    public void setSimpleLiteral(String simpleLiteral) {
        this.simpleLiteral = simpleLiteral;
    }

    public String getExplicitDatatype() {
        return explicitDatatype;
    }

    public void setExplicitDatatype(String explicitDatatype) {
        this.explicitDatatype = explicitDatatype;
    }

    public ZoneOffset getWithConverter() {
        return withConverter;
    }

    public void setWithConverter(ZoneOffset withConverter) {
        this.withConverter = withConverter;
    }

    public OneOfEnum getObjectOneOfEnumAttribute() {
        return objectOneOfEnumAttribute;
    }

    public void setObjectOneOfEnumAttribute(OneOfEnum objectOneOfEnumAttribute) {
        this.objectOneOfEnumAttribute = objectOneOfEnumAttribute;
    }

    @Override
    public String toString() {
        return "OWLClassM{" +
                "key='" + key + '\'' +
                ", booleanAttribute=" + booleanAttribute +
                ", intAttribute=" + intAttribute +
                ", longAttribute=" + longAttribute +
                ", doubleAttribute=" + doubleAttribute +
                ", dateAttribute=" + dateAttribute +
                ", characterAttribute=" + characterAttribute +
                ", enumAttribute=" + enumAttribute +
                ", ordinalEnumAttribute=" + ordinalEnumAttribute +
                ", integerSet=" + integerSet +
                ", lexicalForm=" + lexicalForm +
                ", simpleLiteral=" + simpleLiteral +
                ", explicitDatatype=" + explicitDatatype +
                ", withConverter=" + withConverter +
                ", objectOneOfEnumAttribute=" + objectOneOfEnumAttribute +
                '}';
    }

    public void initializeTestValues(boolean includingKey) {
        if (includingKey) {
            this.key = Vocabulary.CLASS_BASE + "entityM";
        }
        this.booleanAttribute = true;
        this.intAttribute = 117;
        this.longAttribute = 365L;
        this.doubleAttribute = 3.14D;
        this.dateAttribute = new Date();
        this.characterAttribute = 'j';
        this.enumAttribute = Severity.MEDIUM;
        this.ordinalEnumAttribute = enumAttribute;
        this.integerSet = IntStream.generate(Generators::randomInt).limit(10).boxed().collect(Collectors.toSet());
        this.lexicalForm = "test";
        this.simpleLiteral = "test";
        this.explicitDatatype = "P1Y";
        this.withConverter = ZoneOffset.UTC;
        this.objectOneOfEnumAttribute = OneOfEnum.OBJECT_PROPERTY;
    }

    public static String getClassIri() {
        return OWLClassM.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return OWLClassM.class.getDeclaredField("key");
    }

    public static Field getBooleanAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("booleanAttribute");
    }

    public static Field getIntAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("intAttribute");
    }

    public static PropertyInfo getIntAttributeFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getIntAttributeField());
    }

    public static Field getLongAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("longAttribute");
    }

    public static Field getDoubleAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("doubleAttribute");
    }

    public static Field getDateAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("dateAttribute");
    }

    public static PropertyInfo getDateAttributeFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getDateAttributeField());
    }

    public static Field getCharacterAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("characterAttribute");
    }

    public static PropertyInfo getCharacterAttributeFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getCharacterAttributeField());
    }

    public static Field getEnumAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("enumAttribute");
    }

    public static PropertyInfo getEnumAttributePropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.class.getDeclaredField("enumAttribute"));
    }

    public static PropertyInfo getEnumAttributeFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getEnumAttributeField());
    }

    public static Field getOrdinalEnumAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("ordinalEnumAttribute");
    }

    public static PropertyInfo getOrdinalEnumAttributePropertyInfo() throws Exception {
        return PropertyInfo.from(getOrdinalEnumAttributeField());
    }

    public static Field getIntegerSetField() throws Exception {
        return OWLClassM.class.getDeclaredField("integerSet");
    }

    public static PropertyInfo getIntegerSetFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getIntegerSetField());
    }

    public static Field getLexicalFormField() throws Exception {
        return OWLClassM.class.getDeclaredField("lexicalForm");
    }

    public static PropertyInfo getLexicalFormFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getLexicalFormField());
    }

    public static Field getSimpleLiteralField() throws Exception {
        return OWLClassM.class.getDeclaredField("simpleLiteral");
    }

    public static PropertyInfo getSimpleLiteralFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getSimpleLiteralField());
    }

    public static Field getExplicitDatatypeField() throws Exception {
        return OWLClassM.class.getDeclaredField("explicitDatatype");
    }

    public static PropertyInfo getExplicitDatatypeFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getExplicitDatatypeField());
    }

    public static Field getWithConverterField() throws Exception {
        return OWLClassM.class.getDeclaredField("withConverter");
    }

    public static PropertyInfo getWithConverterFieldPropertyInfo() throws Exception {
        return PropertyInfo.from(OWLClassM.getWithConverterField());
    }

    public static Field getObjectOneOfEnumAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("objectOneOfEnumAttribute");
    }
    public static PropertyInfo getObjectOneOfEnumAttributePropertyInfo() throws Exception {
        return PropertyInfo.from(getObjectOneOfEnumAttributeField());
    }
}
