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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class InstanceDescriptorFactoryTest {

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void createNotLoadedCreatesInstanceDescriptorWithAttributesInNotLoadedState() {
        final OWLClassM instance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> result = InstanceDescriptorFactory
                .createNotLoaded(instance, metamodelMocks.forOwlClassM().entityType());
        assertNotNull(result);
        metamodelMocks.forOwlClassM().entityType().getAttributes()
                      .forEach(att -> assertEquals(LoadState.NOT_LOADED, result.isLoaded(att)));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassM().identifier()));
    }

    @Test
    void createAllLoadedCreatesInstanceDescriptorWithAllAttributesInLoadedState() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final InstanceDescriptor<OWLClassA> result = InstanceDescriptorFactory
                .createAllLoaded(instance, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        metamodelMocks.forOwlClassA().entityType().getFieldSpecifications()
                      .forEach(fs -> assertEquals(LoadState.LOADED, result.isLoaded(fs)));
    }

    @Test
    void createCreatesInstanceDescriptorWithLoadedSetForNonNullAttributes() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setTypes(null);
        final InstanceDescriptor<OWLClassA> result = InstanceDescriptorFactory
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
        final InstanceDescriptor<OWLClassA> result = InstanceDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassA().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().stringAttribute()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassA().typesSpec()));
    }

    @Test
    void createCreatesInstanceDescriptorWithUnknownSetForNullLazilyFetchedAttributes() {
        final OWLClassC instance = new OWLClassC(Generators.createIndividualIdentifier());
        final InstanceDescriptor<OWLClassC> result = InstanceDescriptorFactory
                .create(instance, metamodelMocks.forOwlClassC().entityType());
        assertNotNull(result);
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().identifier()));
        assertEquals(LoadState.LOADED, result.isLoaded(metamodelMocks.forOwlClassC().referencedListAtt()));
        assertEquals(LoadState.UNKNOWN, result.isLoaded(metamodelMocks.forOwlClassC().simpleListAtt()));
    }

    @Test
    void createCopyCopiesStateFromSpecifiedDescriptor() {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        final InstanceDescriptor<OWLClassA> original = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType());

        final OWLClassA otherInstance = Generators.generateOwlClassAInstance();
        final InstanceDescriptor<OWLClassA> result = InstanceDescriptorFactory.createCopy(otherInstance, original);
        assertNotNull(result);
        assertSame(otherInstance, result.getInstance());
        assertEquals(original.isLoaded(), result.isLoaded());
    }
}