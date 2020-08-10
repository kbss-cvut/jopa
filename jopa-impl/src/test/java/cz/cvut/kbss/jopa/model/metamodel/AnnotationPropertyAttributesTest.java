/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AnnotationPropertyAttributesTest {

    @Mock
    private FieldMappingValidator validator;

    @Mock
    private MetamodelBuilder metamodelBuilder;

    @Mock
    private TypeBuilderContext<?> typeBuilderContext;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        doAnswer(invocation -> invocation.getArguments()[0]).when(typeBuilderContext).resolveNamespace(anyString());
    }

    @Test
    void resolveInvokesAnnotationPropertyFieldValidation() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassN.getAnnotationPropertyField(), metamodelBuilder,
                OWLClassN.getAnnotationPropertyField().getType());
        verify(validator).validateAnnotationPropertyField(OWLClassN.getAnnotationPropertyField(),
                OWLClassN.getAnnotationPropertyField().getAnnotation(OWLAnnotationProperty.class));
    }

    @Test
    void resolveResolvesLexicalFormConfigurationFromAnnotation() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(WithLexicalForm.class.getDeclaredField("lexicalForm"), metamodelBuilder, String.class);
        assertTrue(sut.isLexicalForm());
    }

    @SuppressWarnings("unused")
    private static class WithLexicalForm {
        @OWLAnnotationProperty(iri = Vocabulary.p_m_lexicalForm, lexicalForm = true)
        private String lexicalForm;
    }

    @Test
    void resolveResolvesSimpleLiteralConfigurationFromAnnotation() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(WithSimpleLiteral.class.getDeclaredField("simpleLiteral"), metamodelBuilder, String.class);
        assertTrue(sut.isSimpleLiteral());
    }

    @SuppressWarnings("unused")
    private static class WithSimpleLiteral {
        @OWLAnnotationProperty(iri = Vocabulary.p_m_simpleLiteral, simpleLiteral = true)
        private String simpleLiteral;
    }

    @Test
    void resolveSetsLanguageFromPersistenceUnitLanguageConfiguration() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        when(typeBuilderContext.getPuLanguage()).thenReturn("en");
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassN.getAnnotationPropertyField(), metamodelBuilder, OWLClassN.getAnnotationPropertyField().getType());
        assertEquals("en", sut.getLanguage());
    }

    @Test
    void resolveSetsLanguageToNullWhenFieldIsMultilingualString() throws Exception {
        final AnnotationPropertyAttributes sut = new AnnotationPropertyAttributes(validator);
        when(typeBuilderContext.getPuLanguage()).thenReturn("en");
        sut.typeBuilderContext = typeBuilderContext;
        sut.resolve(OWLClassN.getAnnotationPropertyField(), metamodelBuilder, MultilingualString.class);
        assertNull(sut.getLanguage());
    }
}
