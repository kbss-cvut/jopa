/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class HintTest {

    private static final String HINT_NAME = "testHint";

    @Mock
    private AbstractQuery query;

    @Mock
    private Statement statement;

    static class TestHint extends QueryHintsHandler.Hint {

        TestHint(Object defaultValue, Object[][] values) {
            super(HINT_NAME, defaultValue);
            this.valueArray = values;
        }

        @Override
        void applyToQuery(Object hintValue, AbstractQuery query, Statement statement) {

        }
    }

    @Test
    void initializeMapsValueTransformation() {
        final TestHint sut = new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                });
        sut.initialize();
        assertTrue(sut.valueMap.containsKey(Boolean.toString(true).toUpperCase(Locale.ROOT)));
        assertTrue(sut.valueMap.containsKey(Boolean.toString(false).toUpperCase(Locale.ROOT)));
        assertEquals(sut.valueMap.get(Boolean.toString(true).toUpperCase(Locale.ROOT)), Boolean.TRUE);
        assertEquals(sut.valueMap.get(Boolean.toString(false).toUpperCase(Locale.ROOT)), Boolean.FALSE);
        assertNull(sut.valueArray);
        assertNull(sut.defaultValue);
    }

    @Test
    void applyAppliesSpecifiedValueToQuery() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply(Boolean.TRUE, query, statement);
        verify(sut).applyToQuery(Boolean.TRUE, query, statement);
    }

    @Test
    void applyTransformsProvidedValueToCorrespondingMappedValue() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply("false", query, statement);
        verify(sut).applyToQuery(Boolean.FALSE, query, statement);
    }

    @Test
    void applyUsesDefaultValueWhenProvidedValueIsEmptyString() {
        final Boolean defaultValue = Boolean.TRUE;
        final TestHint sut = spy(new TestHint(defaultValue, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply("", query, statement);
        verify(sut).applyToQuery(defaultValue, query, statement);
    }

    @Test
    void applyThrowsIllegalArgumentExceptionWhenProvidedValueCannotBeTransformedToExpectedValue() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        assertThrows(IllegalArgumentException.class, () -> sut.apply("not boolean", query, statement));
    }

    @Test
    void applyOnClassFindsCorrectHintInstanceAndAppliesIt() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        QueryHintsHandler.Hint.registerHint(sut);
        QueryHintsHandler.Hint.apply(HINT_NAME, "true", query, statement);
        verify(sut).apply("true", query, statement);
    }

    @Test
    void applyOnClassDoesNothingForUnknownHintName() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        QueryHintsHandler.Hint.registerHint(sut);
        QueryHintsHandler.Hint.apply("unknownHintName", "true", query, statement);
        verify(sut, never()).apply(any(), any(), any());
    }

    @Test
    void disableInferenceHintDisablesInferenceOnStatement() {
        final QueryHintsHandler.Hint sut = new QueryHintsHandler.DisableInferenceHint();
        sut.apply(true, query, statement);
        verify(statement).disableInference();
    }

    @Test
    void disableInferenceHintDisablesInferenceOnDescriptorForTypedQuery() {
        final TypedQueryImpl<OWLClassA> tq = mock(TypedQueryImpl.class);
        final Descriptor descriptor = spy(new EntityDescriptor());
        when(tq.getDescriptor()).thenReturn(descriptor);
        final QueryHintsHandler.Hint sut = new QueryHintsHandler.DisableInferenceHint();
        sut.apply(true, tq, statement);
        verify(descriptor).disableInference();
    }

    @Test
    void setQueryTargetOntologySelectsStatementOntology() {
        final QueryHintsHandler.Hint sut = new QueryHintsHandler.TargetOntologyHint();
        sut.apply(Statement.StatementOntology.TRANSACTIONAL, query, statement);
        verify(statement).useOntology(Statement.StatementOntology.TRANSACTIONAL);
    }
}
