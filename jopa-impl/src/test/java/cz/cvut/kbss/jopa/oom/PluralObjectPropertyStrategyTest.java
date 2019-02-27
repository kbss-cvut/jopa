/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class PluralObjectPropertyStrategyTest {

    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private Descriptor descriptor = new EntityDescriptor();

    private MetamodelMocks mocks;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.mocks = new MetamodelMocks();
    }

    @Test
    void addValueFromAxiomLoadsInstanceAndAddsItIntoAttributeCollection() {
        final Impl<OWLClassJ> sut = strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        final OWLClassA aInstance = new OWLClassA(aReference);
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(aInstance);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNotNull(instance.getOwlClassA());
        assertEquals(1, instance.getOwlClassA().size());
        assertTrue(instance.getOwlClassA().contains(aInstance));
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    private <T> Impl<T> strategy(EntityType<T> et, AbstractPluralAttribute<T, Set, ?> att) {
        return new Impl<>(et, att, descriptor, mapperMock);
    }

    @Test
    void addValueFromAxiomDoesNothingWhenInstanceCannotBeLoaded() {
        final Impl<OWLClassJ> sut = strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        final OWLClassJ instance = new OWLClassJ(ID);
        final URI aReference = Generators.createIndividualIdentifier();
        when(mapperMock.getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class)))
                .thenReturn(null);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID),
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), false), new Value<>(
                NamedResource.create(aReference)));

        sut.addValueFromAxiom(axiom);
        sut.buildInstanceFieldValue(instance);
        assertNull(instance.getOwlClassA());
        verify(mapperMock).getEntityFromCacheOrOntology(eq(OWLClassA.class), eq(aReference), any(Descriptor.class));
    }

    private static class Impl<X> extends PluralObjectPropertyStrategy<AbstractPluralAttribute<? super X, ?, ?>, X> {

        Impl(EntityType<X> et, AbstractPluralAttribute<? super X, Set, ?> att, Descriptor descriptor,
             EntityMappingHelper mapper) {
            super(et, att, descriptor, mapper);
        }

        @Override
        void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
            // Do nothing
        }
    }
}