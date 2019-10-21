/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;

class FieldMappingValidatorTest {

    private FieldMappingValidator validator = new FieldMappingValidator();

    @Test
    void nonMapPropertiesFieldThrowsException() {
        assertThrows(InvalidFieldMappingException.class, () -> validator.validatePropertiesField(getField("values")));
    }

    private static Field getField(String name) throws Exception {
        return InvalidClass.class.getDeclaredField(name);
    }

    @Test
    void rawPropertiesMapThrowsException() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validatePropertiesField(getField("rawProperties")));
    }

    @Test
    void propertiesFieldRequiresValueTypeToBeSet() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validatePropertiesField(getField("propertiesWithIntegerValue")));
    }

    @Test
    void propertiesFieldWithInvalidKeyTypeThrowsException() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validatePropertiesField(getField("propertiesWithInvalidKey")));
    }

    @Test
    void validPropertiesFieldPassesValidation() throws Exception {
        validator.validatePropertiesField(getField("validProperties"));
    }

    @Test
    void nonSetTypesFieldThrowsException() {
        assertThrows(InvalidFieldMappingException.class, () -> validator.validateTypesField(getField("typesList")));
    }

    @Test
    void rawTypesSetThrowsException() {
        assertThrows(InvalidFieldMappingException.class, () -> validator.validateTypesField(getField("rawTypes")));
    }

    @Test
    void invalidTypesValueTypeThrowsException() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateTypesField(getField("invalidValueTypes")));
    }

    @Test
    void setOfUrisIsValidTypesField() throws Exception {
        validator.validateTypesField(getField("validTypes"));
    }

    @Test
    void uriIsValidIdentifierField() throws Exception {
        validator.validateIdentifierType(getField("validUriIdentifier").getType());
    }

    @Test
    void urlIsValidIdentifierField() throws Exception {
        validator.validateIdentifierType(getField("validUrlIdentifier").getType());
    }

    @Test
    void stringIsValidIdentifierField() throws Exception {
        validator.validateIdentifierType(getField("validStringIdentifier").getType());
    }

    @Test
    void invalidIdentifierTypeThrowsException() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateIdentifierType(getField("invalidIdentifier").getType()));
    }

    @Test
    void lexicalFormAnnotationIsValidOnStringField() throws Exception {
        validator.validateDataPropertyField(OWLClassM.getLexicalFormField(),
                OWLClassM.getLexicalFormField().getAnnotation(OWLDataProperty.class));
    }

    @Test
    void validateDataPropertyFieldInvokesLexicalFormValidation() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateDataPropertyField(getField("invalidLexicalForm"),
                        getField("invalidLexicalForm").getAnnotation(OWLDataProperty.class)));
    }

    @Test
    void validateAnnotationPropertyFieldInvokesLexicalFormValidation() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateAnnotationPropertyField(getField("invalidLexicalFormAnnotation"),
                        getField("invalidLexicalFormAnnotation").getAnnotation(OWLAnnotationProperty.class)));
    }

    @Test
    void simpleLiteralAnnotationIsValidOnStringField() throws Exception {
        validator.validateDataPropertyField(OWLClassM.getSimpleLiteralField(),
                OWLClassM.getSimpleLiteralField().getAnnotation(OWLDataProperty.class));
    }

    @Test
    void simpleLiteralAnnotationThrowsInvalidFieldMappingExceptionOnNonStringField() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateDataPropertyField(getField("invalidSimpleLiteral"),
                        getField("invalidSimpleLiteral").getAnnotation(OWLDataProperty.class)));
    }

    @Test
    void simpleLiteralAnnotationThrowsInvalidFieldMappingExceptionOnNonStringAnnotationPropertyField() {
        assertThrows(InvalidFieldMappingException.class,
                () -> validator.validateAnnotationPropertyField(getField("invalidSimpleLiteralAnnotation"),
                        getField("invalidSimpleLiteralAnnotation").getAnnotation(OWLAnnotationProperty.class)));
    }

    @Test
    void simpleLiteralAnnotationIsValidOnSetStringField() throws Exception {
        validator.validateAnnotationPropertyField(getField("validSimpleLiteralSet"),
                getField("validSimpleLiteralSet").getAnnotation(OWLAnnotationProperty.class));
    }

    @Test
    void lexicalFormDataPropertyIsValidOnListStringFields() throws Exception {
        validator.validateDataPropertyField(getField("validLexicalFormList"),
                getField("validLexicalFormList").getAnnotation(OWLDataProperty.class));
    }

    @SuppressWarnings("unused")
    private static final class InvalidClass {

        // Properties tests

        @Properties
        private Set<String> values;

        @Properties
        private Map rawProperties;

        @Properties
        private Map<String, Integer> propertiesWithIntegerValue;

        @Properties
        private Map<Long, Set<String>> propertiesWithInvalidKey;

        @Properties
        private Map<URI, Set<Object>> validProperties;

        // Types tests

        @Types
        private List<String> typesList;

        @Types
        private Set rawTypes;

        @Types
        private Set<Integer> invalidValueTypes;

        @Types
        private Set<URI> validTypes;

        URI validUriIdentifier;

        URL validUrlIdentifier;

        String validStringIdentifier;

        Integer invalidIdentifier;

        @OWLDataProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private Integer invalidLexicalForm;

        @OWLAnnotationProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private Integer invalidLexicalFormAnnotation;

        @OWLDataProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private Integer invalidSimpleLiteral;

        @OWLAnnotationProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private Integer invalidSimpleLiteralAnnotation;

        @OWLAnnotationProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private Set<String> validSimpleLiteralSet;

        @OWLDataProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private List<String> validLexicalFormList;
    }
}
