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
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.EnumType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DataPropertyAttributesTest {

    @Mock
    private FieldMappingValidator validator;

    @Mock
    private MetamodelBuilder metamodelBuilder;

    @Mock
    private TypeBuilderContext<?> typeBuilderContext;

    @BeforeEach
    void setUp() {
        doAnswer(invocation -> invocation.getArguments()[0]).when(typeBuilderContext).resolveNamespace(anyString());
    }

    private DataPropertyAttributes initSystemUnderTest() {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        return sut;
    }

    @Test
    void resolveResolvesLexicalFormConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getLexicalFormFieldPropertyInfo(), metamodelBuilder, OWLClassM.getLexicalFormField().getType());
        assertTrue(sut.isLexicalForm());
    }

    @Test
    void resolveResolvesSimpleLiteralConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getSimpleLiteralFieldPropertyInfo(), metamodelBuilder, OWLClassM.getSimpleLiteralField().getType());
        assertTrue(sut.isSimpleLiteral());
    }

    @Test
    void resolveSetsLanguageFromPersistenceUnitLanguageConfiguration() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        when(typeBuilderContext.getPuLanguage()).thenReturn("en");
        sut.resolve(OWLClassA.getStrAttFieldPropertyInfo(), metamodelBuilder, OWLClassA.getStrAttField().getType());
        assertEquals("en", sut.getLanguage());
    }

    @Test
    void resolveSetsLanguageToNullWhenFieldIsMultilingualString() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassA.getStrAttFieldPropertyInfo(), metamodelBuilder, MultilingualString.class);
        assertNull(sut.getLanguage());
    }

    @Test
    void resolveSetsDatatypeToValueSpecifiedInAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getExplicitDatatypeFieldPropertyInfo(), metamodelBuilder, String.class);
        assertNotNull(sut.getDatatype());
        assertEquals(OWLClassM.getExplicitDatatypeField().getAnnotation(OWLDataProperty.class)
                .datatype(), sut.getDatatype());
    }

    @Test
    void resolveSetsEnumTypeToValueSpecifiedInEnumeratedAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getOrdinalEnumAttributePropertyInfo(), metamodelBuilder, OWLClassM.Severity.class);
        assertEquals(EnumType.ORDINAL, sut.getEnumType());
    }

    @Test
    void resolveSetsEnumTypeToStringWhenEnumeratedAnnotationIsNotSpecifiedOnEnumValuedField() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getEnumAttributePropertyInfo(), metamodelBuilder, OWLClassM.Severity.class);
        assertEquals(EnumType.STRING, sut.getEnumType());
    }

    @Test
    void resolveSetsFetchTypeAlwaysToEager() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(PropertyInfo.from(ClassWithLazyDataProperty.class.getDeclaredField("integers")), metamodelBuilder, Set.class);

        assertEquals(FetchType.EAGER, sut.getFetchType());
    }

    private static final class ClassWithLazyDataProperty {

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "lazyIntegers", fetch = FetchType.LAZY)
        private Set<Integer> integers;
    }
}
