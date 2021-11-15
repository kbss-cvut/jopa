/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class DataPropertyAttributesTest {

    @Mock
    private FieldMappingValidator validator;

    @Mock
    private MetamodelBuilder metamodelBuilder;

    @Mock
    private TypeBuilderContext<?> typeBuilderContext;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        doAnswer(invocation -> invocation.getArguments()[0]).when(typeBuilderContext).resolveNamespace(anyString());
    }

    @Test
    void resolveInvokesDataPropertyFieldValidation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassA.getStrAttField(), metamodelBuilder, OWLClassA.getStrAttField().getType());
        verify(validator)
                .validateDataPropertyField(OWLClassA.getStrAttField(), OWLClassA.getStrAttField().getAnnotation(
                        OWLDataProperty.class));
    }

    private DataPropertyAttributes initSystemUnderTest() {
        final DataPropertyAttributes sut = new DataPropertyAttributes(validator);
        sut.typeBuilderContext = typeBuilderContext;
        return sut;
    }

    @Test
    void resolveResolvesLexicalFormConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getLexicalFormField(), metamodelBuilder, OWLClassM.getLexicalFormField().getType());
        assertTrue(sut.isLexicalForm());
    }

    @Test
    void resolveResolvesSimpleLiteralConfigurationFromAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getSimpleLiteralField(), metamodelBuilder, OWLClassM.getSimpleLiteralField().getType());
        assertTrue(sut.isSimpleLiteral());
    }

    @Test
    void resolveSetsLanguageFromPersistenceUnitLanguageConfiguration() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        when(typeBuilderContext.getPuLanguage()).thenReturn("en");
        sut.resolve(OWLClassA.getStrAttField(), metamodelBuilder, OWLClassA.getStrAttField().getType());
        assertEquals("en", sut.getLanguage());
    }

    @Test
    void resolveSetsLanguageToNullWhenFieldIsMultilingualString() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        when(typeBuilderContext.getPuLanguage()).thenReturn("en");
        sut.resolve(OWLClassA.getStrAttField(), metamodelBuilder, MultilingualString.class);
        assertNull(sut.getLanguage());
    }

    @Test
    void resolveSetsDatatypeToValueSpecifiedInAnnotation() throws Exception {
        final DataPropertyAttributes sut = initSystemUnderTest();
        sut.resolve(OWLClassM.getExplicitDatatypeField(), metamodelBuilder, String.class);
        assertNotNull(sut.getDatatype());
        assertEquals(OWLClassM.getExplicitDatatypeField().getAnnotation(OWLDataProperty.class).datatype(), sut.getDatatype());
    }
}
