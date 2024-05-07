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
package cz.cvut.kbss.jopa.sessions.descriptor;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

class LoadStateDescriptorFactoryTest {

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void createNotLoadedCreatesInstanceDescriptorWithAttributesInNotLoadedState() {
        final OWLClassM instance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> result = LoadStateDescriptorFactory
                .createNotLoaded(instance, metamodelMocks.forOwlClassM().entityType());
        assertNotNull(result);
        metamodelMocks.forOwlClassM().entityType().getAttributes()
                      .forEach(att -> assertEquals(LoadState.NOT_LOADED, result.isLoaded(att)));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassM().identifier()));
    }

    @Test
    void createAllLoadedCreatesInstanceDescriptorWithAllAttributesInLoadedState() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final LoadStateDescriptor<OWLClassA> result = LoadStateDescriptorFactory
                .createAllLoaded(instance, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        metamodelMocks.forOwlClassA().entityType().getFieldSpecifications()
                      .forEach(fs -> assertEquals(LoadState.LOADED, result.isLoaded(fs)));
    }

    @Test
    void createCreatesInstanceDescriptorWithLoadedSetForNonNullAttributes() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(null);
        final LoadStateDescriptor<OWLClassA> result = LoadStateDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().stringAttribute()));
    }

    @Test
    void createCreatesInstanceDescriptorWithLoadedSetForNullEagerlyFetchedAttributes() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setStringAttribute(null);
        instance.setTypes(null);
        final LoadStateDescriptor<OWLClassA> result = LoadStateDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().stringAttribute()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().typesSpec()));
    }

    @Test
    void createCreatesInstanceDescriptorWithUnknownSetForNullLazilyFetchedAttributes() {
        final OWLClassC instance = new OWLClassC(Generators.createIndividualIdentifier());
        final LoadStateDescriptor<OWLClassC> result = LoadStateDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassC().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().referencedListAtt()));
        assertEquals(LoadState.UNKNOWN, result.isLoaded(metamodelMocks.forOwlClassC().simpleListAtt()));
    }

    @Test
    void createCopyCopiesStateFromSpecifiedDescriptor() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final LoadStateDescriptor<OWLClassA> original = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType());

        final OWLClassA otherInstance = Generators.generateOwlClassAInstance();
        final LoadStateDescriptor<OWLClassA> result = LoadStateDescriptorFactory.createCopy(otherInstance, original);
        assertNotNull(result);
        assertSame(otherInstance, result.getInstance());
        assertEquals(original.isLoaded(), result.isLoaded());
    }

    @Test
    void createCreatesDescriptorWithNotLoadedForLazyLoadedAttributesContainingProxyInstances() {
        final OWLClassC instance = new OWLClassC(Generators.createIndividualIdentifier());
        instance.setSimpleList(mock(LazyLoadingListProxy.class));
        final LoadStateDescriptor<OWLClassC> result = LoadStateDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassC().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().referencedListAtt()));
        assertEquals(LoadState.NOT_LOADED, result.isLoaded(metamodelMocks.forOwlClassC().simpleListAtt()));
    }
}
