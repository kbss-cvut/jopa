/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

class MultilingualStringInstanceBuilderTest {

    private final Descriptor descriptor = new EntityDescriptor();

    private MultilingualStringInstanceBuilder sut;

    @BeforeEach
    void setUp() {
        this.sut = new MultilingualStringInstanceBuilder(mock(CloneBuilder.class), mock(UnitOfWork.class));
    }

    @Test
    void buildCloneCopiesAllTranslationsInSpecifiedOriginalMultilingualString() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        original.set("cs", "stavba");
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), original,
                new CloneConfiguration(descriptor, false));
        assertThat(result, instanceOf(MultilingualString.class));
        final MultilingualString typedResult = (MultilingualString) result;
        assertEquals(original.getValue(), typedResult.getValue());
    }

    @Test
    void buildCloneResultsNullWhenOriginalIsNull() throws Exception {
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), null,
                new CloneConfiguration(descriptor, false));
        assertNull(result);
    }

    @Test
    void buildCloneReturnsIndirectWrapperAllowingToTrackModifyingOperations() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        original.set("cs", "stavba");
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), original,
                new CloneConfiguration(descriptor, false));
        assertThat(result, instanceOf(ChangeTrackingIndirectMultilingualString.class));
    }

    @Test
    void buildCloneBuildsCloneOfWrappedMultilingualStringWhenArgumentIsIndirectMultilingualString() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        final ChangeTrackingIndirectMultilingualString arg = new ChangeTrackingIndirectMultilingualString(new OWLClassU(),
                                                                                                          OWLClassU.getSingularStringAttField(), mock(UnitOfWork.class), original);
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), arg, new CloneConfiguration(descriptor, false));
        assertThat(result, instanceOf(ChangeTrackingIndirectMultilingualString.class));
        final ChangeTrackingIndirectMultilingualString resultIndirect = (ChangeTrackingIndirectMultilingualString) result;
        assertEquals(original, resultIndirect.unwrap());
    }

    @Test
    void mergeChangesCopiesValueOfCloneToOriginal() throws Exception {
        final MultilingualString clone = MultilingualString.create("building", Generators.LANG);
        clone.set("cs", "stavba");
        final MultilingualString original = MultilingualString.create("construction", Generators.LANG);
        final OWLClassU target = new OWLClassU();
        target.setSingularStringAtt(original);
        sut.mergeChanges(OWLClassU.getSingularStringAttField(), target, original, clone);
        assertEquals(clone.getValue(), target.getSingularStringAtt().getValue());
    }

    @Test
    void mergeChangesSupportsCloneBeingNull() throws Exception {
        final MultilingualString original = MultilingualString.create("construction", Generators.LANG);
        final OWLClassU target = new OWLClassU();
        target.setSingularStringAtt(original);
        sut.mergeChanges(OWLClassU.getSingularStringAttField(), target, original, null);
        assertNull(target.getSingularStringAtt());
    }
}
