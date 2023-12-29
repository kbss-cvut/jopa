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
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ReferenceSavingResolverTest {

    @Mock
    private ObjectOntologyMapperImpl mapperMock;

    private MetamodelMocks metamodelMocks;

    private ReferenceSavingResolver resolver;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        this.resolver = new ReferenceSavingResolver(mapperMock);
    }

    @Test
    void shouldSaveReferenceReturnsTrueForNullValue() {
        assertTrue(resolver.shouldSaveReference(null, Collections.emptySet()));
    }

    @Test
    void shouldSaveReferenceReturnsTrueForPlainIdentifierValue() {
        final URI value = Generators.createIndividualIdentifier();
        assertTrue(resolver.shouldSaveReference(value, Collections.emptySet()));
    }

    @Test
    void shouldSaveReferenceReturnsFalseForValueWithNullIdentifier() {
        // Because it means it is not managed and does not exist in the storage either
        final OWLClassA value = new OWLClassA();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        assertFalse(resolver.shouldSaveReference(value, Collections.emptySet()));
    }

    @Test
    void shouldSaveReferenceReturnsTrueForValueBeingManaged() {
        final OWLClassA value = Generators.generateOwlClassAInstance();
        when(mapperMock.isManaged(value)).thenReturn(true);
        assertTrue(resolver.shouldSaveReference(value, Collections.emptySet()));
        verify(mapperMock).isManaged(value);
    }

    @Test
    void shouldSaveReferenceReturnsTrueForValueExistingInStorage() {
        final OWLClassA value = Generators.generateOwlClassAInstance();
        when(mapperMock.isManaged(value)).thenReturn(false);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(mapperMock.containsEntity(eq(OWLClassA.class), eq(value.getUri()), any())).thenReturn(true);
        assertTrue(resolver.shouldSaveReference(value, Collections.emptySet()));
        verify(mapperMock).containsEntity(OWLClassA.class, value.getUri(), new EntityDescriptor());
    }

    @Test
    void shouldSaveReferenceReturnsTrueForValueExistingInStorageContext() {
        final OWLClassA value = Generators.generateOwlClassAInstance();
        final URI context = Generators.createIndividualIdentifier();
        when(mapperMock.isManaged(value)).thenReturn(false);
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(metamodelMocks.forOwlClassA().entityType());
        when(mapperMock.containsEntity(eq(OWLClassA.class), eq(value.getUri()), any())).thenReturn(true);
        assertTrue(resolver.shouldSaveReference(value, Collections.singleton(context)));
        verify(mapperMock).containsEntity(OWLClassA.class, value.getUri(), new EntityDescriptor(context));
    }

    @Test
    void registerPendingReferenceRegistersReferenceInMapper() {
        final NamedResource subject = NamedResource.create(Generators.createIndividualIdentifier());
        final Assertion assertion =
                Assertion.createObjectPropertyAssertion(Generators.createPropertyIdentifier(), false);
        final OWLClassA object = new OWLClassA();
        resolver.registerPendingReference(subject, assertion, object, null);
        verify(mapperMock).registerPendingAssertion(subject, assertion, object, null);
    }

    @Test
    void shouldSaveReferenceReturnsTrueForEnumValue() {
        assertTrue(resolver.shouldSaveReference(OneOfEnum.OBJECT_PROPERTY, Collections.emptySet()));
    }

    @Test
    void shouldSaveReferenceToItemReturnsTrueForEnumValue() {
        assertTrue(resolver.shouldSaveReferenceToItem(OneOfEnum.DATATYPE_PROPERTY, Collections.emptySet()));
    }
}
