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
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.LoadState;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class LoadStateDescriptorTest {

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void constructorInitializesAllDataPropertyAttributesWithSpecifiedState() {
        final OWLClassM instance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType(), LoadState.UNKNOWN);
        metamodelMocks.forOwlClassM().entityType().getAttributes()
                      .forEach(fs -> assertEquals(LoadState.UNKNOWN, sut.isLoaded(fs)));
    }

    @Test
    void constructorSetsIdentifierAsLoaded() {
        final OWLClassM instance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType(), LoadState.NOT_LOADED);
        assertEquals(LoadState.LOADED, sut.isLoaded(metamodelMocks.forOwlClassM().identifier()));
    }

    @Test
    void copyConstructorCreatesDescriptorForDifferentInstanceWithSameLoadedState() {
        final OWLClassM instance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType(), LoadState.UNKNOWN);
        sut.setLoaded(metamodelMocks.forOwlClassM().booleanAttribute(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassM().integerAttribute(), LoadState.LOADED);

        final OWLClassM anotherInstance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> result = new LoadStateDescriptor<>(anotherInstance, sut);
        metamodelMocks.forOwlClassM().entityType().getFieldSpecifications()
                      .forEach(fs -> assertEquals(sut.isLoaded(fs), result.isLoaded(fs)));
        assertSame(anotherInstance, result.getInstance());
        assertEquals(sut.isLoaded(), result.isLoaded());
    }

    @Test
    void isLoadedByAttributeReturnsLoadedAllEagerAttributesAreLoaded() {
        final OWLClassA instance = new OWLClassA();
        final LoadStateDescriptor<OWLClassA> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType(), LoadState.UNKNOWN);
        sut.setLoaded(metamodelMocks.forOwlClassA().identifier(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassA().stringAttribute(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassA().typesSpec(), LoadState.LOADED);
        assertEquals(LoadState.LOADED, sut.isLoaded(metamodelMocks.forOwlClassA().stringAttribute()));
    }

    @Test
    void isLoadedReturnsNotLoadedWhenAtLeastOneEagerlyFetchedAttributeIsNotLoaded() {
        final OWLClassA instance = new OWLClassA();
        final LoadStateDescriptor<OWLClassA> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType(), LoadState.NOT_LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassA().stringAttribute(), LoadState.LOADED);
        // types are eagerly fetched and are not loaded

        assertEquals(LoadState.NOT_LOADED, sut.isLoaded());
    }

    @Test
    void isLoadedReturnsUnknownWhenAtLeastOneEagerlyFetchedAttributeIsUnknownAndOthersAreLoaded() {
        final OWLClassM instance = new OWLClassM();
        final LoadStateDescriptor<OWLClassM> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType(), LoadState.UNKNOWN);
        metamodelMocks.forOwlClassM().entityType().getFieldSpecifications()
                      .forEach(fs -> sut.setLoaded(fs, LoadState.LOADED));
        sut.setLoaded(metamodelMocks.forOwlClassM().booleanAttribute(), LoadState.UNKNOWN);

        assertEquals(LoadState.UNKNOWN, sut.isLoaded());
    }

    @Test
    void isLoadedReturnsLoadedWhenEagerlyFetchedAttributesAreLoadedAndLazilyIsNotLoaded() {
        final OWLClassC instance = new OWLClassC();
        final LoadStateDescriptor<OWLClassC> sut = new LoadStateDescriptor<>(instance,
                metamodelMocks.forOwlClassC().entityType(), LoadState.UNKNOWN);
        sut.setLoaded(metamodelMocks.forOwlClassC().referencedListAtt(), LoadState.LOADED);

        assertEquals(LoadState.LOADED, sut.isLoaded());
    }
}
