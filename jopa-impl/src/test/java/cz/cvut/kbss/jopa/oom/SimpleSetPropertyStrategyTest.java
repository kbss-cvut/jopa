/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class SimpleSetPropertyStrategyTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");


    @Mock
    private EntityMappingHelper mapperMock;

    @Mock
    private CascadeResolver cascadeResolverMock;

    private MetamodelMocks mocks;

    private SimpleSetPropertyStrategy<OWLClassJ> strategy;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        final Descriptor descriptor = new EntityDescriptor();
        this.strategy = new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(),
                mocks.forOwlClassJ().setAttribute(), descriptor, mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
    }

    @Test
    public void extractsValuesFromInstance() throws Exception {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(PK);
        j.setOwlClassA(generateSet());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(j, builder);
        final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(NamedResource.create(PK), res.getSubject());
        final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
                OWLObjectProperty.class);
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
                OWLClassJ.getOwlClassAField().getAnnotation(Inferred.class) != null);
        assertEquals(j.getOwlClassA().size(), res.getAssertionValues(ass).size());
        for (OWLClassA aa : j.getOwlClassA()) {
            assertTrue(res.getAssertionValues(ass).contains(new Value<>(NamedResource.create(aa.getUri()))));
        }
        verify(cascadeResolverMock, times(j.getOwlClassA().size())).resolveFieldCascading(
                eq(mocks.forOwlClassJ().setAttribute()), any(Object.class), eq((URI) null));
    }

    private Set<OWLClassA> generateSet() {
        final Set<OWLClassA> set = new HashSet<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
            set.add(a);
        }
        return set;
    }

    @Test
    public void extractsValuesFromInstanceSetIsNull() throws Exception {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(PK);
        j.setOwlClassA(null);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(j, builder);
        final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(NamedResource.create(PK), res.getSubject());
        final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
                OWLObjectProperty.class);
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
                OWLClassJ.getOwlClassAField().getAnnotation(Inferred.class) != null);
        assertEquals(1, res.getAssertionValues(ass).size());
        assertSame(Value.nullValue(), res.getAssertionValues(ass).get(0));
    }

    @Test
    public void throwsExceptionWhenMinimumCardinalityConstraintIsViolated() throws Exception {
        final Set<OWLClassA> set = generateSet();
        // TODO
    }
}
