/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.exception.InvalidConverterException;
import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.Convert;
import cz.cvut.kbss.jopa.model.annotations.EnumType;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.oom.converter.*;
import cz.cvut.kbss.jopa.oom.converter.datetime.DateConverter;
import cz.cvut.kbss.jopa.oom.converter.datetime.InstantConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.time.Instant;
import java.time.ZoneOffset;
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
        final PropertyInfo propertyInfo = ClassWithObjectAnnotation.getsingularAnnotationPropertyInfo();
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

        public static PropertyInfo getsingularAnnotationPropertyInfo() throws NoSuchFieldException, SecurityException {
            return PropertyInfo.from(ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation"));
        }
    }

    @Test
    void resolveConverterUsesConfigurationToProvideSettingsToObjectConverter() throws Exception {
        final Configuration config = new Configuration();
        config.set(JOPAPersistenceProperties.PREFER_MULTILINGUAL_STRING, Boolean.TRUE.toString());
        sut = new ConverterResolver(new Converters(config));
        final PropertyInfo propertyInfo = ClassWithObjectAnnotation.getsingularAnnotationPropertyInfo();
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
}
