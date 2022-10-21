/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.annotations.Convert;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
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

    @OWLDataProperty(iri = Vocabulary.p_m_enumAttribute)
    private Severity enumAttribute;

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

    @Override
    public String toString() {
        return "OWLCLassM{" +
                "key='" + key + '\'' +
                ", booleanAttribute=" + booleanAttribute +
                ", intAttribute=" + intAttribute +
                ", longAttribute=" + longAttribute +
                ", doubleAttribute=" + doubleAttribute +
                ", enumAttribute=" + enumAttribute +
                ", integerSet=" + integerSet +
                ", lexicalForm=" + lexicalForm +
                ", simpleLiteral=" + simpleLiteral +
                ", explicitDatatype=" + explicitDatatype +
                ", withConverter=" + withConverter +
                '}';
    }

    public void initializeTestValues(boolean includingKey) {
        if (includingKey) {
            this.key = "http://krizik.felk.cvut.cz/ontologies/entityM";
        }
        this.booleanAttribute = true;
        this.intAttribute = 117;
        this.longAttribute = 365L;
        this.doubleAttribute = 3.14D;
        this.dateAttribute = new Date();
        this.enumAttribute = Severity.MEDIUM;
        this.integerSet = IntStream.generate(Generators::randomInt).limit(10).boxed().collect(Collectors.toSet());
        this.lexicalForm = "test";
        this.simpleLiteral = "test";
        this.explicitDatatype = "P1Y";
        this.withConverter = ZoneOffset.UTC;
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

    public static Field getLongAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("longAttribute");
    }

    public static Field getDoubleAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("doubleAttribute");
    }

    public static Field getDateAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("dateAttribute");
    }

    public static Field getEnumAttributeField() throws Exception {
        return OWLClassM.class.getDeclaredField("enumAttribute");
    }

    public static Field getIntegerSetField() throws Exception {
        return OWLClassM.class.getDeclaredField("integerSet");
    }

    public static Field getLexicalFormField() throws Exception {
        return OWLClassM.class.getDeclaredField("lexicalForm");
    }

    public static Field getSimpleLiteralField() throws Exception {
        return OWLClassM.class.getDeclaredField("simpleLiteral");
    }

    public static Field getExplicitDatatypeField() throws Exception {
        return OWLClassM.class.getDeclaredField("explicitDatatype");
    }

    public static Field getWithConverterField() throws Exception {
        return OWLClassM.class.getDeclaredField("withConverter");
    }
}
