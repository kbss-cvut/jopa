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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static cz.cvut.kbss.jopa.oom.EntityConstructorTest.getClassAssertionAxiomForType;
import static cz.cvut.kbss.jopa.oom.EntityConstructorTest.getStringAttAssertionAxiom;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class EntityConstructorQueryAttributesTest {

    private static final URI IDENTIFIER = Generators.createIndividualIdentifier();

    @Mock
    private ObjectOntologyMapperImpl mapperMock;
    @Mock
    private UnitOfWorkImpl uowMock;

    @Mock
    private TypedQueryImpl<?> typedQueryMock;

    @Mock
    private SparqlQueryFactory queryFactoryMock;

    private MetamodelMocks mocks;
    private Descriptor descriptor;

    private EntityConstructor constructor;

    @BeforeEach
    void setUp() throws Exception {
        when(mapperMock.getConfiguration()).thenReturn(new Configuration(Collections.emptyMap()));
        when(mapperMock.getUow()).thenReturn(uowMock);
        when(uowMock.sparqlQueryFactory()).thenReturn(queryFactoryMock);
        this.mocks = new MetamodelMocks();
        this.descriptor = new EntityDescriptor();
        this.constructor = new EntityConstructor(mapperMock);
    }

    @Test
    void testReconstructEntityWithQueryAttribute() throws Exception {
        final String stringValue = "String value";
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(IDENTIFIER, OWLClassWithQueryAttr.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(IDENTIFIER, stringValue, OWLClassWithQueryAttr.getStrAttField()));

        doReturn(typedQueryMock)
                .when(queryFactoryMock).createNativeQuery(any(String.class),
                        (Class<?>) any(Class.class));
        doReturn(typedQueryMock)
                .when(typedQueryMock).setParameter(any(String.class), any());
        doReturn(stringValue).when(typedQueryMock).getSingleResult();

        final OWLClassWithQueryAttr res = constructor.reconstructEntity(IDENTIFIER, mocks.forOwlClassWithQueryAttr()
                .entityType(), descriptor, axioms);
        assertNotNull(res);
        assertEquals(IDENTIFIER, res.getUri());
        assertEquals(stringValue, res.getStringAttribute());
        assertEquals(stringValue, res.getStringQueryAttribute());
        verify(mapperMock).registerInstance(IDENTIFIER, res);
    }

    @Test
    void testReconstructEntityWithManagedTypeQueryAttribute() throws Exception {
        final String stringValue = "String value";
        final URI identifierTwo = Generators.createIndividualIdentifier();
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(IDENTIFIER, OWLClassWithQueryAttr.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(IDENTIFIER, stringValue, OWLClassWithQueryAttr.getStrAttField()));

        final URI assertionUri = URI.create(OWLClassWithQueryAttr.getEntityAttField()
                .getAnnotation(OWLObjectProperty.class).iri());
        final Axiom<NamedResource> opAssertion = new AxiomImpl<>(NamedResource.create(IDENTIFIER),
                Assertion.createObjectPropertyAssertion(assertionUri, false),
                new Value<>(NamedResource.create(identifierTwo)));
        axioms.add(opAssertion);

        final Descriptor fieldDesc = new EntityDescriptor();
        descriptor.addAttributeDescriptor(mocks.forOwlClassWithQueryAttr().entityAttribute(), fieldDesc);
        final OWLClassA entityA = new OWLClassA();
        entityA.setUri(identifierTwo);
        entityA.setStringAttribute(stringValue);

        when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, identifierTwo, fieldDesc))
                .thenReturn(entityA);

        doReturn(typedQueryMock)
                .when(queryFactoryMock).createNativeQuery(any(String.class),
                        (Class<?>) any(Class.class));
        doReturn(typedQueryMock)
                .when(typedQueryMock).setParameter(any(String.class), any());
        doReturn(entityA).when(typedQueryMock).getSingleResult();

        final OWLClassWithQueryAttr res = constructor.reconstructEntity(IDENTIFIER, mocks.forOwlClassWithQueryAttr()
                .entityType(), descriptor, axioms);
        assertNotNull(res);
        assertEquals(IDENTIFIER, res.getUri());
        verify(mapperMock).getEntityFromCacheOrOntology(OWLClassA.class, identifierTwo, fieldDesc);
        assertNotNull(res.getEntityAttribute());
        assertEquals(identifierTwo, res.getEntityAttribute().getUri());
        assertEquals(stringValue, res.getEntityAttribute().getStringAttribute());

        assertNotNull(res.getEntityQueryAttribute());
        assertEquals(identifierTwo, res.getEntityQueryAttribute().getUri());
        assertEquals(stringValue, res.getEntityQueryAttribute().getStringAttribute());

        verify(mapperMock).registerInstance(IDENTIFIER, res);
    }

    @Test
    void reconstructEntityUsesReferencedEntityAttributeValuesWhenAssemblingQueryForAttribute() throws Exception {
        final String stringValue = "String value";
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(IDENTIFIER, OWLClassWithQueryAttr.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(IDENTIFIER, stringValue, OWLClassWithQueryAttr.getStrAttField()));
        when(mocks.forOwlClassWithQueryAttr().entityQueryAttribute().getFetchType()).thenReturn(FetchType.LAZY);

        doReturn(typedQueryMock).when(queryFactoryMock)
                .createNativeQuery(any(String.class), (Class<?>) any(Class.class));
        doReturn(typedQueryMock).when(typedQueryMock).setParameter(any(String.class), any());
        when(typedQueryMock.hasParameter(anyString())).thenReturn(false);
        when(typedQueryMock.hasParameter(OWLClassWithQueryAttr.getStrAttField().getName())).thenReturn(true);
        doReturn(stringValue).when(typedQueryMock).getSingleResult();

        final OWLClassWithQueryAttr res = constructor.reconstructEntity(IDENTIFIER, mocks.forOwlClassWithQueryAttr()
                .entityType(), descriptor, axioms);
        assertNotNull(res);
        assertEquals(IDENTIFIER, res.getUri());
        assertEquals(stringValue, res.getStringAttribute());
        assertEquals(stringValue, res.getStringQueryAttribute());
        verify(typedQueryMock).setParameter(OWLClassWithQueryAttr.getStrAttField().getName(), stringValue);
    }

    @Test
    void reconstructEntityDoesNotUserReferencedEntityAttributeValuesWhenQueryDisablesIt() throws Exception {
        final String stringValue = "String value";
        final Set<Axiom<?>> axioms = new HashSet<>();
        axioms.add(getClassAssertionAxiomForType(IDENTIFIER, OWLClassWithQueryAttr.getClassIri()));
        axioms.add(getStringAttAssertionAxiom(IDENTIFIER, stringValue, OWLClassWithQueryAttr.getStrAttField()));
        when(mocks.forOwlClassWithQueryAttr().entityQueryAttribute().getFetchType()).thenReturn(FetchType.LAZY);
        when(mocks.forOwlClassWithQueryAttr().stringQueryAttribute().enableReferencingAttributes()).thenReturn(false);

        doReturn(typedQueryMock).when(queryFactoryMock)
                .createNativeQuery(any(String.class), (Class<?>) any(Class.class));
        doReturn(typedQueryMock).when(typedQueryMock).setParameter(any(String.class), any());
        when(typedQueryMock.hasParameter(OWLClassWithQueryAttr.getStrAttField().getName())).thenReturn(true);
        doReturn(stringValue).when(typedQueryMock).getSingleResult();

        final OWLClassWithQueryAttr res = constructor.reconstructEntity(IDENTIFIER, mocks.forOwlClassWithQueryAttr()
                .entityType(), descriptor, axioms);
        assertNotNull(res);
        assertEquals(IDENTIFIER, res.getUri());
        assertEquals(stringValue, res.getStringAttribute());
        assertEquals(stringValue, res.getStringQueryAttribute());
        verify(typedQueryMock, never()).setParameter(anyString(), any());
    }
}
