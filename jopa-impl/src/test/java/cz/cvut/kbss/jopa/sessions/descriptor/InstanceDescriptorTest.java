/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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

class InstanceDescriptorTest {

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void constructorInitializesAllDataPropertyAttributesAsNotLoaded() {
        final OWLClassM instance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType());
        metamodelMocks.forOwlClassM().entityType().getAttributes()
                      .forEach(fs -> assertEquals(LoadState.NOT_LOADED, sut.isLoaded(fs)));
    }

    @Test
    void constructorSetsIdentifierAsLoaded() {
        final OWLClassM instance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType());
        assertEquals(LoadState.LOADED, sut.isLoaded(metamodelMocks.forOwlClassM().identifier()));
    }

    @Test
    void copyConstructorCreatesDescriptorForDifferentInstanceWithSameLoadedState() {
        final OWLClassM instance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType());
        sut.setLoaded(metamodelMocks.forOwlClassM().booleanAttribute(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassM().integerAttribute(), LoadState.LOADED);

        final OWLClassM anotherInstance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> result = new InstanceDescriptor<>(anotherInstance, sut);
        metamodelMocks.forOwlClassM().entityType().getFieldSpecifications()
                      .forEach(fs -> assertEquals(sut.isLoaded(fs), result.isLoaded(fs)));
        assertSame(anotherInstance, result.getInstance());
        assertEquals(sut.isLoaded(), result.isLoaded());
    }

    @Test
    void isLoadedByAttributeReturnsLoadedAllEagerAttributesAreLoaded() {
        final OWLClassA instance = new OWLClassA();
        final InstanceDescriptor<OWLClassA> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType());
        sut.setLoaded(metamodelMocks.forOwlClassA().identifier(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassA().stringAttribute(), LoadState.LOADED);
        sut.setLoaded(metamodelMocks.forOwlClassA().typesSpec(), LoadState.LOADED);
        assertEquals(LoadState.LOADED, sut.isLoaded(metamodelMocks.forOwlClassA().stringAttribute()));
    }

    @Test
    void isLoadedReturnsNotLoadedWhenAtLeastOneEagerlyFetchedAttributeIsNotLoaded() {
        final OWLClassA instance = new OWLClassA();
        final InstanceDescriptor<OWLClassA> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassA().entityType());
        sut.setLoaded(metamodelMocks.forOwlClassA().stringAttribute(), LoadState.LOADED);
        // types are eagerly fetched and are not loaded

        assertEquals(LoadState.NOT_LOADED, sut.isLoaded());
    }

    @Test
    void isLoadedReturnsUnknownWhenAtLeastOneEagerlyFetchedAttributeIsUnknownAndOthersAreLoaded() {
        final OWLClassM instance = new OWLClassM();
        final InstanceDescriptor<OWLClassM> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassM().entityType());
        metamodelMocks.forOwlClassM().entityType().getFieldSpecifications()
                      .forEach(fs -> sut.setLoaded(fs, LoadState.LOADED));
        sut.setLoaded(metamodelMocks.forOwlClassM().booleanAttribute(), LoadState.UNKNOWN);

        assertEquals(LoadState.UNKNOWN, sut.isLoaded());
    }

    @Test
    void isLoadedReturnsLoadedWhenEagerlyFetchedAttributesAreLoadedAndLazilyIsNotLoaded() {
        final OWLClassC instance = new OWLClassC();
        final InstanceDescriptor<OWLClassC> sut = new InstanceDescriptor<>(instance,
                metamodelMocks.forOwlClassC().entityType());
        sut.setLoaded(metamodelMocks.forOwlClassC().referencedListAtt(), LoadState.LOADED);

        assertEquals(LoadState.LOADED, sut.isLoaded());
    }
}