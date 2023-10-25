/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("unchecked")
@ExtendWith(MockitoExtension.class)
class InferredAttributeChangeValidatorTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generators.createIndividualIdentifier());

    @Mock
    private ConnectionWrapper connectionWrapper;

    @InjectMocks
    private InferredAttributeChangeValidator sut;

    @Test
    void validateChangeExtractsAxiomsForAttributeAndChecksWhetherRemovedOnesAreNotInferred() {
        final OWLClassM original = new OWLClassM();
        original.initializeTestValues(false);
        original.setKey(SUBJECT.toString());
        final OWLClassM clone = new OWLClassM();
        clone.setKey(SUBJECT.toString());
        clone.setIntegerSet(new HashSet<>(original.getIntegerSet()));
        final Integer toRemove = original.getIntegerSet().iterator().next();
        clone.getIntegerSet().remove(toRemove);
        final FieldSpecification<? super OWLClassM, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        final Assertion a = Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), true);
        doAnswer(inv -> {
            final OWLClassM instance = inv.getArgument(0, OWLClassM.class);
            return instance.getIntegerSet().stream().map(i -> new AxiomImpl<>(SUBJECT, a, new Value<>(i))).collect(
                    Collectors.toSet());
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());

        sut.validateChange(clone, original, fieldSpec, descriptor);
        verify(connectionWrapper).getAttributeAxioms(clone, fieldSpec, descriptor);
        verify(connectionWrapper).getAttributeAxioms(original, fieldSpec, descriptor);
        verify(connectionWrapper).isInferred(new AxiomImpl<>(SUBJECT, a, new Value<>(toRemove)),
                                             Collections.emptySet());
    }

    @Test
    void validateChangeThrowsInferredAttributeModifiedExceptionWhenRemovedAxiomIsInferred() {
        final OWLClassM original = new OWLClassM();
        original.initializeTestValues(false);
        original.setKey(SUBJECT.toString());
        final OWLClassM clone = new OWLClassM();
        clone.setKey(SUBJECT.toString());
        clone.setIntegerSet(new HashSet<>(original.getIntegerSet()));
        final Integer toRemove = original.getIntegerSet().iterator().next();
        clone.getIntegerSet().remove(toRemove);
        final FieldSpecification<? super OWLClassM, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        final Assertion a = Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), true);
        doAnswer(inv -> {
            final OWLClassM instance = inv.getArgument(0, OWLClassM.class);
            return instance.getIntegerSet().stream().map(i -> new AxiomImpl<>(SUBJECT, a, new Value<>(i))).collect(
                    Collectors.toSet());
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());
        when(connectionWrapper.isInferred(new AxiomImpl<>(SUBJECT, a, new Value<>(toRemove)),
                                          Collections.emptySet())).thenReturn(true);

        final InferredAttributeModifiedException ex = assertThrows(InferredAttributeModifiedException.class,
                                                                   () -> sut.validateChange(clone, original, fieldSpec,
                                                                                            descriptor));
        assertThat(ex.getMessage(), containsString(
                "Value " + toRemove + " of attribute " + fieldSpec + " is inferred and cannot be removed"));
    }

    @Test
    void validateChangeThrowsInferredAttributeModifiedExceptionWhenSingularAttributeIsModified() {
        final OWLClassF original = new OWLClassF(SUBJECT.getIdentifier());
        original.setSecondStringAttribute("Something original");
        final OWLClassF clone = new OWLClassF(original.getUri());
        clone.setSecondStringAttribute("Something different");
        final FieldSpecification<? super OWLClassF, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        doAnswer(inv -> {
            final OWLClassF instance = inv.getArgument(0, OWLClassF.class);
            return Collections.singleton(new AxiomImpl<>(SUBJECT,
                                                         Assertion.createDataPropertyAssertion(
                                                                 URI.create("http://F-secondStringAttribute"),
                                                                 true),
                                                         new Value<>(instance.getSecondStringAttribute())));
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());
        when(connectionWrapper.isInferred(any(Axiom.class), eq(Collections.emptySet()))).thenReturn(true);

        assertThrows(InferredAttributeModifiedException.class,
                     () -> sut.validateChange(clone, original, fieldSpec, descriptor));
    }

    @Test
    void validateChangeAllowsAdditiveChangeOfSingularAttribute() {
        final OWLClassF original = new OWLClassF(SUBJECT.getIdentifier());
        original.setSecondStringAttribute(null);
        final OWLClassF clone = new OWLClassF(original.getUri());
        clone.setSecondStringAttribute("Something different");
        final FieldSpecification<? super OWLClassF, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        doAnswer(inv -> {
            final OWLClassF instance = inv.getArgument(0, OWLClassF.class);
            if (instance.getSecondStringAttribute() == null) {
                return Collections.emptySet();
            }
            return Collections.singleton(new AxiomImpl<>(SUBJECT,
                                                         Assertion.createDataPropertyAssertion(
                                                                 URI.create(Vocabulary.p_f_stringAttribute), true),
                                                         new Value<>(instance.getSecondStringAttribute())));
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());

        assertDoesNotThrow(() -> sut.validateChange(clone, original, fieldSpec, descriptor));
    }

    @Test
    void validateChangeAllowsAdditiveChangeOfPluralAttribute() {
        final OWLClassM original = new OWLClassM();
        original.initializeTestValues(false);
        original.setKey(SUBJECT.toString());
        final OWLClassM clone = new OWLClassM();
        clone.setKey(SUBJECT.toString());
        clone.setIntegerSet(new HashSet<>(original.getIntegerSet()));
        clone.getIntegerSet().add(Generators.randomInt());
        final FieldSpecification<? super OWLClassM, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        final Assertion a = Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_m_IntegerSet), true);
        doAnswer(inv -> {
            final OWLClassM instance = inv.getArgument(0, OWLClassM.class);
            return instance.getIntegerSet().stream().map(i -> new AxiomImpl<>(SUBJECT, a, new Value<>(i))).collect(
                    Collectors.toSet());
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());

        assertDoesNotThrow(() -> sut.validateChange(clone, original, fieldSpec, descriptor));
    }

    @Test
    void isInferredValueRemovalReturnsTrueWhenChangeRemovesInferredValue() {
        final OWLClassF original = new OWLClassF(SUBJECT.getIdentifier());
        original.setSecondStringAttribute("Something original");
        final OWLClassF clone = new OWLClassF(original.getUri());
        clone.setSecondStringAttribute("Something different");
        final FieldSpecification<? super OWLClassF, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        doAnswer(inv -> {
            final OWLClassF instance = inv.getArgument(0, OWLClassF.class);
            return Collections.singleton(new AxiomImpl<>(SUBJECT,
                    Assertion.createDataPropertyAssertion(
                            URI.create("http://F-secondStringAttribute"),
                            true),
                    new Value<>(instance.getSecondStringAttribute())));
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());
        when(connectionWrapper.isInferred(any(Axiom.class), eq(Collections.emptySet()))).thenReturn(true);

        assertTrue(sut.isInferredValueRemoval(clone, original, fieldSpec, descriptor));
    }

    @Test
    void insInferredValueRemovalReturnsFalseWhenChangeToInferredAttributeIsAdditive() {
        final OWLClassF original = new OWLClassF(SUBJECT.getIdentifier());
        original.setSecondStringAttribute(null);
        final OWLClassF clone = new OWLClassF(original.getUri());
        clone.setSecondStringAttribute("Something different");
        final FieldSpecification<? super OWLClassF, ?> fieldSpec = mock(FieldSpecification.class);
        final Descriptor descriptor = new EntityDescriptor();
        doAnswer(inv -> {
            final OWLClassF instance = inv.getArgument(0, OWLClassF.class);
            if (instance.getSecondStringAttribute() == null) {
                return Collections.emptySet();
            }
            return Collections.singleton(new AxiomImpl<>(SUBJECT,
                    Assertion.createDataPropertyAssertion(
                            URI.create(Vocabulary.p_f_stringAttribute), true),
                    new Value<>(instance.getSecondStringAttribute())));
        }).when(connectionWrapper).getAttributeAxioms(any(), any(), any());

        assertFalse(sut.isInferredValueRemoval(clone, original, fieldSpec, descriptor));
    }
}
