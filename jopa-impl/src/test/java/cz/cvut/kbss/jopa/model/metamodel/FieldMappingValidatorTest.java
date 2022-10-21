/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.DefaultConverterWrapper;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class FieldMappingValidatorTest {

    private final FieldMappingValidator validator = new FieldMappingValidator();

    @Test
    void nonMapPropertiesFieldThrowsException() {
        assertThrows(InvalidFieldMappingException.class, () -> validator.validatePropertiesField(getField("values")));
    }

    private static Field getField(String name) throws Exception {
        return TestClass.class.getDeclaredField(name);
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
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isLexicalForm()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_lexicalForm));
        when(attribute.getJavaField()).thenReturn(OWLClassM.getLexicalFormField());
        when(attribute.getJavaType()).thenReturn(OWLClassM.getLexicalFormField().getType());
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void validateLiteralFieldMappingThrowsInvalidFieldMappingExceptionWhenFieldIsLexicalFormAndNotString() throws Exception {
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isLexicalForm()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_lexicalForm));
        when(attribute.getJavaField()).thenReturn(getField("invalidLexicalForm"));
        when(attribute.getJavaType()).thenReturn(getField("invalidLexicalForm").getType());
        assertThrows(InvalidFieldMappingException.class, () -> validator.validateAttributeMapping(attribute));
    }

    @Test
    void simpleLiteralAnnotationIsValidOnStringField() throws Exception {
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isSimpleLiteral()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(attribute.getJavaType()).thenReturn(OWLClassM.getSimpleLiteralField().getType());
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void validateLiteralFieldMappingThrowsInvalidFieldMappingExceptionWhenFieldIsSimpleLiteralAndNotString() throws Exception {
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isSimpleLiteral()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(attribute.getJavaField()).thenReturn(getField("invalidSimpleLiteral"));
        when(attribute.getJavaType()).thenReturn(getField("invalidSimpleLiteral").getType());
        when(attribute.getConverter()).thenReturn(DefaultConverterWrapper.INSTANCE);
        assertThrows(InvalidFieldMappingException.class, () -> validator.validateAttributeMapping(attribute));
    }

    @Test
    void validateLiteralFieldMappingAllowsSimpleLiteralMappingViaConverterSupportingStringAxiomValue() throws Exception {
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isSimpleLiteral()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(attribute.getJavaField()).thenReturn(getField("invalidSimpleLiteral"));
        when(attribute.getJavaType()).thenReturn(getField("invalidSimpleLiteral").getType());
        final ConverterWrapper<Integer, String> wrapper = mock(ConverterWrapper.class);
        when(wrapper.supportsAxiomValueType(String.class)).thenReturn(true);
        when(attribute.getConverter()).thenReturn(wrapper);
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void simpleLiteralAnnotationIsValidOnSetStringField() throws Exception {
        final AbstractPluralAttribute attribute = mock(AbstractPluralAttribute.class);
        when(attribute.isCollection()).thenReturn(true);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(attribute.isSimpleLiteral()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(attribute.getJavaField()).thenReturn(getField("validSimpleLiteralSet"));
        when(attribute.getBindableJavaType()).thenReturn(String.class);
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void simpleLiteralMappingIsValidOnEnum() throws Exception {
        final AbstractAttribute attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(attribute.isSimpleLiteral()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_simpleLiteral));
        when(attribute.getJavaField()).thenReturn(getField("validEnumSimpleLiteral"));
        when(attribute.getJavaType()).thenReturn(getField("validEnumSimpleLiteral").getType());
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void lexicalFormDataPropertyIsValidOnListStringFields() throws Exception {
        final AbstractPluralAttribute attribute = mock(AbstractPluralAttribute.class);
        when(attribute.isCollection()).thenReturn(true);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.isLexicalForm()).thenReturn(true);
        when(attribute.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_lexicalForm));
        when(attribute.getJavaField()).thenReturn(OWLClassM.getLexicalFormField());
        when(attribute.getBindableJavaType()).thenReturn(String.class);
        validator.validateAttributeMapping(attribute);
    }

    @Test
    void validateAttributeMappingThrowsInvalidFieldMappingExceptionWhenFieldMapsRDFType() throws Exception {
        final AbstractAttribute<?, ?> attribute = mock(AbstractAttribute.class);
        when(attribute.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(attribute.getIRI()).thenReturn(IRI.create(RDF.TYPE));
        when(attribute.getJavaField()).thenReturn(getField("type"));
        assertThrows(InvalidFieldMappingException.class, () -> validator.validateAttributeMapping(attribute));
    }

    @SuppressWarnings("unused")
    private static final class TestClass {

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

        @OWLDataProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private Integer invalidSimpleLiteral;

        @OWLAnnotationProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private Set<String> validSimpleLiteralSet;

        @OWLDataProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private List<String> validLexicalFormList;

        @OWLDataProperty(iri = RDF.TYPE)
        private String type;

        @OWLDataProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private OWLClassM.Severity validEnumSimpleLiteral;
    }
}
