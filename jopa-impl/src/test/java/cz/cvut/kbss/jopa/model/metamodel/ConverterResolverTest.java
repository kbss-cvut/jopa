/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.ZoneOffsetConverter;
import cz.cvut.kbss.jopa.exception.InvalidConverterException;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.Convert;
import cz.cvut.kbss.jopa.model.annotations.EnumType;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.CustomConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.ObjectConverter;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.OrdinalEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.StringEnumConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.oom.converter.ToRdfLiteralConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.DateConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.InstantConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Optional;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ConverterResolverTest {

    private ConverterResolver sut = new ConverterResolver(new Converters(new Configuration()));

    @Test
    void resolveConverterReturnsEmptyOptionalForObjectPropertyWithEntityTarget() throws Exception {
        final PropertyInfo propertyInfo = OWLClassD.getOwlClassAFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final EntityType et = mock(EntityType.class);
        when(et.getJavaType()).thenReturn(OWLClassA.class);
        when(pa.getType()).thenReturn(et);
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertFalse(result.isPresent());
    }

    @Test
    void resolveConverterReturnsBuiltInIntegerConverterForIntegerDataPropertyField() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getIntAttributeFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Integer.class));
        assertThat(result.get(), instanceOf(ToIntegerConverter.class));
    }

    @Test
    void resolveConverterReturnsBuiltInDateConverterForDataPropertyWithDateTarget() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getDateAttributeFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Date.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Literal.class));
        assertThat(result.get(), instanceOf(DateConverter.class));
    }

    @Test
    void resolveConverterReturnsBuiltInInstantConverterForInstantDataPropertyField() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getDateAttributeFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Instant.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(InstantConverter.class));
    }

    @Test
    void resolveConverterReturnsBuiltInIntegerConverterForPluralIntegerDataPropertyField() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getIntegerSetFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Integer.class));
        assertThat(result.get(), instanceOf(ToIntegerConverter.class));
    }

    @Test
    void resolveConverterReturnsBuiltInStringEnumConverterForStringEnumDataPropertyField() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getEnumAttributeFieldPropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(pa.getEnumType()).thenReturn(EnumType.STRING);
        doReturn(BasicTypeImpl.get(OWLClassM.Severity.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertInstanceOf(StringEnumConverter.class, result.get());
    }

    @Test
    void resolveConverterReturnsToLexicalFormConverterForFieldWithLexicalForm() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getLexicalFormFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(pa.isLexicalForm()).thenReturn(true);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(ToLexicalFormConverter.class));
    }

    @Test
    void resolveConverterReturnsObjectConverterForFieldOfObjectType() throws Exception {
        final PropertyInfo propertyInfo = ClassWithObjectAnnotation.getSingularAnnotationPropertyInfo();
        final AnnotationPropertyAttributes pa = mock(AnnotationPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        doReturn(BasicTypeImpl.get(Object.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(ObjectConverter.class));
    }

    private static class ClassWithObjectAnnotation {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "singularAnnotation")
        private Object singularAnnotation;

        public static PropertyInfo getSingularAnnotationPropertyInfo() throws NoSuchFieldException, SecurityException {
            return PropertyInfo.from(ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation"));
        }
    }

    @Test
    void resolveConverterUsesConfigurationToProvideSettingsToObjectConverter() throws Exception {
        final Configuration config = new Configuration();
        config.set(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING, Boolean.TRUE.toString());
        sut = new ConverterResolver(new Converters(config));
        final PropertyInfo propertyInfo = ClassWithObjectAnnotation.getSingularAnnotationPropertyInfo();
        final AnnotationPropertyAttributes pa = mock(AnnotationPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        doReturn(BasicTypeImpl.get(Object.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(ObjectConverter.class));
        final ObjectConverter objectConverter = (ObjectConverter) result.get();
        assertTrue(objectConverter.doesPreferMultilingualString());
    }

    @Test
    void resolveConverterReturnsToRdfLiteralConverterForAttributeWithExplicitDatatypeMapping() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getExplicitDatatypeFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(true);
        when(pa.getDatatype()).thenReturn(XSD.DURATION);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(ToRdfLiteralConverter.class));
    }

    @Test
    void resolveConverterThrowsInvalidFieldMappingExceptionWhenFieldWithExplicitDatatypeIsNotOfTypeString() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getIntAttributeFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(true);
        when(pa.getDatatype()).thenReturn(XSD.DURATION);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        assertThrows(InvalidFieldMappingException.class, () -> sut.resolveConverter(propertyInfo, pa));
    }

    @Test
    void resolveConverterReturnsCustomConverterInstanceSpecifiedByConvertAnnotation() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getWithConverterFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(false);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(ZoneOffset.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertThat(result.get(), instanceOf(CustomConverterWrapper.class));
        assertThat(((CustomConverterWrapper<?, ?>) result.get()).getWrappedConverter(),
                   instanceOf(ZoneOffsetConverter.class));
        assertTrue(result.get().supportsAxiomValueType(String.class));
    }

    @Test
    void resolveConverterReturnsOptionalWhenConvertAnnotationDisablesConversion() throws Exception {
        final PropertyInfo propertyInfo = ClassWithDisabledConverter.getZoneOffsetFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(false);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(ZoneOffset.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertFalse(result.isPresent());
    }

    private static class ClassWithDisabledConverter {
        @Convert(disableConversion = true)
        @OWLDataProperty(iri = Vocabulary.p_m_withConverter)
        private ZoneOffset zoneOffset;

        @Convert(converter = ConverterResolver.class)
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "with-bad-converter")
        private ZoneOffset badConverter;

        public static PropertyInfo getZoneOffsetFieldPropertyInfo() throws Exception {
            return PropertyInfo.from(ClassWithDisabledConverter.class.getDeclaredField("zoneOffset"));
        }
        public static PropertyInfo getBadConverterFieldPropertyInfo() throws Exception {
            return PropertyInfo.from(ClassWithDisabledConverter.class.getDeclaredField("badConverter"));
        }
    }

    @Test
    void resolveConverterThrowsInvalidConverterExceptionWhenConverterDoesNotImplementAttributeConverter() throws Exception {
        final PropertyInfo propertyInfo = ClassWithDisabledConverter.getBadConverterFieldPropertyInfo();
        final DataPropertyAttributes pa = mock(DataPropertyAttributes.class);
        when(pa.hasDatatype()).thenReturn(false);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(ZoneOffset.class)).when(pa).getType();
        assertThrows(InvalidConverterException.class, () -> sut.resolveConverter(propertyInfo, pa));
    }

    @Test
    void resolveConverterReturnsObjectOneOfEnumConverterForEnumValuedObjectPropertyAttribute() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getObjectOneOfEnumAttributePropertyInfo();
        final ObjectPropertyAttributes pa = mock(ObjectPropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(pa.getEnumType()).thenReturn(EnumType.OBJECT_ONE_OF);
        doReturn(BasicTypeImpl.get(OneOfEnum.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertInstanceOf(ObjectOneOfEnumConverter.class, result.get());
    }

    @Test
    void resolveConverterReturnsBuiltInOrdinalEnumConverterForOrdinalEnumDataPropertyField() throws Exception {
        final PropertyInfo propertyInfo = OWLClassM.getOrdinalEnumAttributePropertyInfo();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(pa.getEnumType()).thenReturn(EnumType.ORDINAL);
        doReturn(BasicTypeImpl.get(OWLClassM.Severity.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(propertyInfo, pa);
        assertTrue(result.isPresent());
        assertInstanceOf(OrdinalEnumConverter.class, result.get());
    }

    @Test
    void resolveConverterThrowsInvalidConverterExceptionWhenAttemptingToUseCustomConverterOnObjectPropertyField() throws Exception {
        final PropertyInfo field =PropertyInfo.from(ClassWithConverterOnObjectProperty.class.getDeclaredField("converted"));
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final EntityType et = mock(EntityType.class);
        when(et.getJavaType()).thenReturn(ClassWithConverterOnObjectProperty.class);
        when(pa.getType()).thenReturn(et);
        final InvalidConverterException ex = assertThrows(InvalidConverterException.class, () -> sut.resolveConverter(field, pa));
        assertThat(ex.getMessage(), containsString("object properties"));
    }

    private static class ClassWithConverterOnObjectProperty {
        @Convert(converter = ObjectPropertyConverter.class)
        @OWLObjectProperty(iri = Vocabulary.p_h_hasA)
        private OWLClassA converted;

    }

    public static class ObjectPropertyConverter implements AttributeConverter<OWLClassA, NamedResource> {
        @Override
        public NamedResource convertToAxiomValue(OWLClassA value) {
            return NamedResource.create(value.getUri());
        }

        @Override
        public OWLClassA convertToAttribute(NamedResource value) {
            return new OWLClassA(value.getIdentifier());
        }
    }
}
