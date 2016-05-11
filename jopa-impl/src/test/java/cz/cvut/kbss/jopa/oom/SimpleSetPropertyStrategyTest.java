/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class SimpleSetPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();


    @Mock
    private EntityMappingHelper mapperMock;

    @Mock
    private CascadeResolver cascadeResolverMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private Descriptor descriptor = new EntityDescriptor();

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
    }

    @Test
    public void extractsValuesFromInstance() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(),
                        mocks.forOwlClassJ().setAttribute(), descriptor, mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(generateSet());
        strategy.buildAxiomValuesFromInstance(j, gatherer);
        final Set<URI> expected = j.getOwlClassA().stream().map(OWLClassA::getUri).collect(Collectors.toSet());
        verifyExtractedValues(expected);
    }

    private void verifyExtractedValues(Set<URI> expected) throws Exception {
        final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(NamedResource.create(PK), res.getSubject());
        final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
                OWLObjectProperty.class);
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
                OWLClassJ.getOwlClassAField().getAnnotation(Inferred.class) != null);
        assertEquals(expected.size(), res.getAssertionValues(ass).size());
        for (URI u : expected) {
            assertTrue(res.getAssertionValues(ass).contains(new Value<>(NamedResource.create(u))));
        }
        verify(cascadeResolverMock, times(expected.size())).resolveFieldCascading(
                eq(mocks.forOwlClassJ().setAttribute()), any(Object.class), eq((URI) null));
    }

    @Test
    public void extractValuesSkipsNullItem() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(),
                        mocks.forOwlClassJ().setAttribute(), descriptor, mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(generateSet());
        j.getOwlClassA().add(null);

        strategy.buildAxiomValuesFromInstance(j, gatherer);
        final Set<URI> expected = j.getOwlClassA().stream().filter(item -> item != null).map(OWLClassA::getUri)
                                   .collect(Collectors.toSet());
        verifyExtractedValues(expected);
    }

    private Set<OWLClassA> generateSet() {
        final Set<OWLClassA> set = new HashSet<>();
        for (int i = 0; i < 10; i++) {
            set.add(new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i)));
        }
        return set;
    }

    @Test
    public void extractValuesFromInstanceWhenSetIsNullCreatesNullValueAxiom() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassJ().entityType(),
                        mocks.forOwlClassJ().setAttribute(), descriptor, mapperMock);
        strategy.setCascadeResolver(cascadeResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
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

    private Collection<Axiom<NamedResource>> buildAxiomsForSet(URI property, Set<OWLClassA> set) {
        final NamedResource subject = NamedResource.create(PK);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(property, false);
        return set.stream().map(a -> new AxiomImpl<>(subject, assertion, new Value<>(NamedResource.create(a.getUri()))))
                  .collect(Collectors.toList());
    }

    @Test
    public void buildsInstanceFieldAsSetOfUrls() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassP().entityType(),
                        mocks.forOwlClassP().pUrlsAttribute(), descriptor, mapperMock);
        final URI property =
                URI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri());
        final Set<OWLClassA> values = generateSet();
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForSet(property, values);
        axioms.forEach(strategy::addValueFromAxiom);

        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        assertNotNull(p.getIndividualUrls());
        assertEquals(values.size(), p.getIndividualUrls().size());
        values.forEach(a -> {
            try {
                assertTrue(p.getIndividualUrls().contains(a.getUri().toURL()));
            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
        });
    }

    @Test
    public void extractsValuesFromFieldWithSetOfPlainIdentifiers() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassP().entityType(),
                        mocks.forOwlClassP().pUrlsAttribute(), descriptor, mapperMock);
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        setIndividualUrls(p);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        verifyExtractedValuesForP(p.getIndividualUrls());
    }

    private void verifyExtractedValuesForP(Set<URL> expected) throws Exception {
        final AxiomValueDescriptor axiomDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        final URI property =
                URI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri());
        final List<Value<?>>
                values = axiomDescriptor.getAssertionValues(Assertion.createObjectPropertyAssertion(property, false));
        assertEquals(expected.size(), values.size());
        for (Value<?> v : values) {
            assertTrue(v.getValue() instanceof NamedResource);
            assertTrue(expected.contains(new URL(v.stringValue())));
        }
    }

    private void setIndividualUrls(OWLClassP p) {
        final Set<OWLClassA> aSet = generateSet();
        p.setIndividualUrls(aSet.stream().map(a -> {
            try {
                return a.getUri().toURL();
            } catch (MalformedURLException e) {
                throw new IllegalStateException(e);
            }
        }).collect(Collectors.toSet()));
    }

    @Test
    public void extractValuesSkipsNullItems() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                new SimpleSetPropertyStrategy<>(mocks.forOwlClassP().entityType(),
                        mocks.forOwlClassP().pUrlsAttribute(), descriptor, mapperMock);
        final OWLClassP p = new OWLClassP();
        p.setUri(PK);
        setIndividualUrls(p);
        p.getIndividualUrls().add(null);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final Set<URL> expected = p.getIndividualUrls().stream().filter(item -> item != null)
                                   .collect(Collectors.toSet());
        // Added null should be skipped and the original set of values prepared for save
        verifyExtractedValuesForP(expected);
    }
}
