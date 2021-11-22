/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.oom.converter.*;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Date;
import java.util.Optional;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ConverterResolverTest {

    private ConverterResolver sut = new ConverterResolver(new Converters(new Configuration()));

    @Test
    void resolveConverterReturnsEmptyOptionalForObjectPropertyWithEntityTarget() throws Exception {
        final Field field = OWLClassD.getOwlClassAField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertFalse(result.isPresent());
    }

    @Test
    void resolveConverterReturnsBuiltInIntegerConverterForIntegerDataPropertyField() throws Exception {
        final Field field = OWLClassM.getIntAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Integer.class));
        assertTrue(result.get() instanceof ToIntegerConverter);
    }

    @Test
    void resolveConverterReturnsEmptyOptionalForDataPropertyWithDateTarget() throws Exception {
        final Field field = OWLClassM.getDateAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Date.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertFalse(result.isPresent());
    }

    @Test
    void resolveConverterReturnsBuiltInInstantConverterForInstantDataPropertyField() throws Exception {
        final Field field = OWLClassM.getDateAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Instant.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof InstantConverter);
    }

    @Test
    void resolveConverterReturnsBuiltInIntegerConverterForPluralIntegerDataPropertyField() throws Exception {
        final Field field = OWLClassM.getIntegerSetField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Integer.class));
        assertTrue(result.get() instanceof ToIntegerConverter);
    }

    @Test
    void resolveConverterReturnsBuiltInEnumConverterForEnumDataPropertyField() throws Exception {
        final Field field = OWLClassM.getEnumAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(OWLClassM.Severity.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof EnumConverter);
        assertTrue(result.get().supportsAxiomValueType(String.class));
    }

    @Test
    void resolveConverterReturnsToLexicalFormConverterForFieldWithLexicalForm() throws Exception {
        final Field field = OWLClassM.getLexicalFormField();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(pa.isLexicalForm()).thenReturn(true);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof ToLexicalFormConverter);
    }

    @Test
    void resolveConverterReturnsObjectConverterForFieldOfObjectType() throws Exception {
        final Field field = ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation");
        final AnnotationPropertyAttributes pa = mock(AnnotationPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        doReturn(BasicTypeImpl.get(Object.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof ObjectConverter);
    }

    private static class ClassWithObjectAnnotation {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "singularAnnotation")
        private Object singularAnnotation;
    }

    @Test
    void resolveConverterUsesConfigurationToProvideSettingsToObjectConverter() throws Exception {
        final Configuration config = new Configuration();
        config.set(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING, Boolean.TRUE.toString());
        sut = new ConverterResolver(new Converters(config));
        final Field field = ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation");
        final AnnotationPropertyAttributes pa = mock(AnnotationPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        doReturn(BasicTypeImpl.get(Object.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof ObjectConverter);
        final ObjectConverter objectConverter = (ObjectConverter) result.get();
        assertTrue(objectConverter.doesPreferMultilingualString());
    }

    @Test
    void resolveConverterReturnsToRdfLiteralConverterForAttributeWithExplicitDatatypeMapping() throws Exception {
        final Field field = OWLClassM.getExplicitDatatypeField();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(true);
        when(pa.getDatatype()).thenReturn(XSD.DURATION);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(ToRdfLiteralConverter.class));
    }
}
