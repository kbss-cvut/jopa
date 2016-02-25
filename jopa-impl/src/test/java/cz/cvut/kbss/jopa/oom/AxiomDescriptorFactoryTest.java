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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.net.URI;

import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

public class AxiomDescriptorFactoryTest {

    private static final URI CONTEXT = URI
            .create("http://krizik.felk.cvut.cz/ontologies/contextOne");
    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/entityX");

    private static URI stringAttAUri;
    private static URI stringAttBUri;
    private static URI owlClassAAttUri;

    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor;
    private Descriptor descriptorInContext;

    private AxiomDescriptorFactory factory;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        stringAttAUri = URI.create(OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class)
                                            .iri());
        stringAttBUri = URI.create(OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class)
                                            .iri());
        owlClassAAttUri = URI.create(OWLClassD.getOwlClassAField()
                                              .getAnnotation(OWLObjectProperty.class).iri());
    }

    @Before
    public void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        this.descriptor = new EntityDescriptor();
        this.descriptorInContext = new EntityDescriptor(CONTEXT);

        factory = new AxiomDescriptorFactory();
    }

    @Test
    public void testCreateForEntityLoadingWithTypes() throws Exception {
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    public void testCreateForEntityLoadingWithTypesInContext() throws Exception {
        descriptor.addAttributeContext(OWLClassA.getTypesField(), CONTEXT);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification and the string attribute
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
        assertEquals(CONTEXT, res.getAssertionContext(Assertion.createClassAssertion(false)));
    }

    @Test
    public void testCreateForEntityLoadingWithPropertiesAndContext() throws Exception {
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassB.class, PK, descriptorInContext),
                        metamodelMocks.forOwlClassB().entityType());
        // Class assertion, properties specification and the string attribute
        assertEquals(3, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertEquals(CONTEXT, res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttBUri, false)));
    }

    @Test
    public void testCreateForEntityLoadingWithObjectPropertyInContext() throws Exception {
        descriptor.addAttributeContext(OWLClassD.getOwlClassAField(), CONTEXT);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the object property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        Assertion ass = null;
        for (Assertion a : res.getAssertions()) {
            if (a.getIdentifier().equals(owlClassAAttUri)) {
                ass = a;
                break;
            }
        }
        assertNotNull(ass);
        assertEquals(CONTEXT, res.getAssertionContext(ass));
        assertEquals(owlClassAAttUri, ass.getIdentifier());
    }

    @Test
    public void testCreateForEntityLoadingWithAnnotationProperty() throws Exception {
        // Artificially change the attribute type to annotation
        when(metamodelMocks.forOwlClassD().owlClassAAtt().getPersistentAttributeType()).thenReturn(
                PersistentAttributeType.ANNOTATION);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassD.class, PK, descriptor),
                        metamodelMocks.forOwlClassD().entityType());
        // Class assertion and the annotation property assertion
        assertEquals(2, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertTrue(res.getAssertions().contains(
                Assertion.createAnnotationPropertyAssertion(owlClassAAttUri, false)));
    }

    @Test
    public void createForEntityLoadingWithLazilyLoadedAttribute() throws Exception {
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = factory
                .createForEntityLoading(new LoadingParameters<>(OWLClassA.class, PK, descriptor),
                        metamodelMocks.forOwlClassA().entityType());
        // Types specification (class assertion)
        assertEquals(1, res.getAssertions().size());
        assertEquals(NamedResource.create(PK), res.getSubject());
        assertNull(res.getSubjectContext());
        assertFalse(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
        assertTrue(res.getAssertions().contains(Assertion.createClassAssertion(false)));
    }

    @Test
    public void testCreateForFieldLoadingDataProperty() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        when(metamodelMocks.forOwlClassA().stringAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getStrAttField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(res);
        assertEquals(1, res.getAssertions().size());
        assertTrue(res.getAssertions().contains(
                Assertion.createDataPropertyAssertion(stringAttAUri, false)));
    }

    @Test
    public void testCreateForFieldLoadingObjectPropertyInEntityContext() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        desc.addAttributeDescriptor(OWLClassD.getOwlClassAField(), new EntityDescriptor(CONTEXT));
        final AxiomDescriptor res = factory.createForFieldLoading(PK,
                OWLClassD.getOwlClassAField(), desc, metamodelMocks.forOwlClassD().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createObjectPropertyAssertion(owlClassAAttUri, false), as);
        assertEquals(CONTEXT, res.getAssertionContext(as));
    }

    @Test
    public void testCreateForFieldLoadingTypes() throws Exception {
        final Descriptor desc = new EntityDescriptor(CONTEXT);
        final AxiomDescriptor res = factory.createForFieldLoading(PK, OWLClassA.getTypesField(),
                desc, metamodelMocks.forOwlClassA().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion.createClassAssertion(metamodelMocks.forOwlClassA().typesSpec().isInferred()), as);
        assertEquals(CONTEXT, res.getAssertionContext(as));
    }

    @Test
    public void testCreateForFieldLoadingProperties() throws Exception {
        final Descriptor desc = new EntityDescriptor();
        final AxiomDescriptor res = factory.createForFieldLoading(PK,
                OWLClassB.getPropertiesField(), desc, metamodelMocks.forOwlClassB().entityType());
        assertEquals(1, res.getAssertions().size());
        final Assertion as = res.getAssertions().iterator().next();
        assertEquals(Assertion
                .createUnspecifiedPropertyAssertion(metamodelMocks.forOwlClassB().propertiesSpec().isInferred()),
                as);
    }
}
