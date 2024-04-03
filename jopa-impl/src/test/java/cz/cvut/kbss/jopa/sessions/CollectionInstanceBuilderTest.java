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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectList;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.IntStream;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("unused")
@ExtendWith(MockitoExtension.class)
public class CollectionInstanceBuilderTest {

    @Mock
    private AbstractUnitOfWork uowMock;

    private Descriptor descriptor;

    private CollectionInstanceBuilder builder;

    @BeforeEach
    public void setUp() {
        CloneBuilder cloneBuilder = new CloneBuilder(uowMock);
        this.builder = new CollectionInstanceBuilder(cloneBuilder, uowMock);
        this.descriptor = new EntityDescriptor();
    }

    private void mockIndirectCollectionBuilder() {
        when(uowMock.createIndirectCollection(any(), any(), any(Field.class))).thenAnswer(invocation -> {
            final Collection<?> col = (Collection<?>) invocation.getArguments()[0];
            final Object owner = invocation.getArguments()[1];
            final Field field = (Field) invocation.getArguments()[2];
            if (col instanceof List) {
                return new ChangeTrackingIndirectList<>(owner, field, uowMock, (List<?>) col);
            } else {
                return new ChangeTrackingIndirectSet<>(owner, field, uowMock, (Set<?>) col);
            }
        });
    }

    @Test
    public void buildCloneCreatesDefaultListWhenUnknownListImplementationIsPassedAsArgument() throws Exception {
        mockIndirectCollectionBuilder();
        final CollectionOwner owner = new CollectionOwner();
        owner.list = new TestList<>(owner);
        IntStream.range(0, 10).forEach(i -> owner.list.add("String" + i));
        final Object result =
                builder.buildClone(owner, CollectionOwner.listField(), owner.list, new CloneConfiguration(descriptor, false));
        assertNotNull(result);
        assertInstanceOf(List.class, result);
        final List<?> lstResult = (List<?>) result;
        owner.list.forEach(e -> assertTrue(lstResult.contains(e)));
    }

    @Test
    public void buildCloneCreatesDefaultSetWhenUnknownSetImplementationIsPassedAsArgument() throws Exception {
        mockIndirectCollectionBuilder();
        final CollectionOwner owner = new CollectionOwner();
        owner.set = new TestSet<>(owner);
        IntStream.range(0, 10).forEach(i -> owner.set.add("String" + i));
        final Object result =
                builder.buildClone(owner, CollectionOwner.setField(), owner.set, new CloneConfiguration(descriptor, false));
        assertNotNull(result);
        assertInstanceOf(Set.class, result);
        final Set<?> setResult = (Set<?>) result;
        owner.set.forEach(e -> assertTrue(setResult.contains(e)));
    }

    @Test
    public void buildCloneThrowsUnsupportedCollectionTypeExceptionWhenCollectionIsNeitherListNorSet() {
        final CollectionOwner owner = new CollectionOwner();
        owner.queue = new TestQueue<>(owner);
        final OWLPersistenceException ex = assertThrows(OWLPersistenceException.class, () -> builder
                .buildClone(owner, CollectionOwner.queueField(), owner.queue, new CloneConfiguration(descriptor, false)));
        assertThat(ex.getMessage(),
                   containsString("Cannot clone unsupported collection instance of type " + owner.queue.getClass()));
    }

    private static class CollectionOwner {

        private CollectionOwner() {
        }

        private List<String> list;

        private Set<String> set;

        private Queue<String> queue;

        private static Field listField() throws NoSuchFieldException {
            return CollectionOwner.class.getDeclaredField("list");
        }

        private static Field setField() throws NoSuchFieldException {
            return CollectionOwner.class.getDeclaredField("set");
        }

        private static Field queueField() throws NoSuchFieldException {
            return CollectionOwner.class.getDeclaredField("queue");
        }
    }

    private static class TestList<E> extends ArrayList<E> {

        private final CollectionOwner owner;

        private TestList(CollectionOwner owner) {
            this.owner = owner;
        }
    }

    private static class TestSet<E> extends HashSet<E> {

        private final CollectionOwner owner;

        private TestSet(CollectionOwner owner) {
            this.owner = owner;
        }
    }

    private static class TestQueue<E> extends ArrayDeque<E> {

        private final CollectionOwner owner;

        private TestQueue(CollectionOwner owner) {
            this.owner = owner;
        }
    }

    @Test
    public void buildingSingletonSetCloneRegistersElementCloneInUoW() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassJ owner = new OWLClassJ(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setOwlClassA(Collections.singleton(aOrig));
        when(uowMock.registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);
        final Set<?> clone = (Set<?>) builder.buildClone(owner, OWLClassJ.getOwlClassAField(), owner.getOwlClassA(),
                                                         new CloneConfiguration(descriptor, false));
        assertEquals(owner.getOwlClassA().size(), clone.size());
        assertSame(aClone, clone.iterator().next());
        verify(uowMock).registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor));
    }

    @Test
    void buildingSingletonSetReturnsRegularSet() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassJ owner = new OWLClassJ(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setOwlClassA(Collections.singleton(aOrig));
        when(uowMock.registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);
        final Set<OWLClassA> clone = (Set<OWLClassA>) builder.buildClone(owner, OWLClassJ.getOwlClassAField(), owner.getOwlClassA(),
                new CloneConfiguration(descriptor, false));
        assertDoesNotThrow(() -> clone.add(Generators.generateOwlClassAInstance()));
    }

    @Test
    public void buildCloneClonesSingletonListWithContent() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassC owner = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setSimpleList(Collections.singletonList(aOrig));
        when(uowMock.registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);

        final List<?> clone = (List<?>) builder.buildClone(owner, OWLClassC.getSimpleListField(), owner.getSimpleList(),
                                                           new CloneConfiguration(descriptor, false));
        assertEquals(1, clone.size());
        assertSame(aClone, clone.get(0));
        verify(uowMock).registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor));
    }

    @Test
    public void buildCloneOfSingletonListReturnsRegularList() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassC owner = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setSimpleList(Collections.singletonList(aOrig));
        when(uowMock.registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);

        final List<OWLClassA> clone = (List<OWLClassA>) builder.buildClone(owner, OWLClassC.getSimpleListField(), owner.getSimpleList(),
                new CloneConfiguration(descriptor, false));
        assertDoesNotThrow(() -> clone.add(Generators.generateOwlClassAInstance()));
    }

    @Test
    public void buildCloneOfListOfInstanceReturnsRegularList() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassC owner = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setSimpleList(List.of(aOrig));
        when(uowMock.registerExistingObject(aOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);

        final List<OWLClassA> clone = (List<OWLClassA>) builder.buildClone(owner, OWLClassC.getSimpleListField(), owner.getSimpleList(),
                new CloneConfiguration(descriptor, false));
        assertDoesNotThrow(() -> clone.add(Generators.generateOwlClassAInstance()));
    }

    @Test
    public void buildCloneClonesArrayAsListWithContent() throws Exception {
        mockIndirectCollectionBuilder();
        final OWLClassC owner = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassA aOneOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aOneClone = new OWLClassA(aOneOrig);
        final OWLClassA aTwoOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aTwoClone = new OWLClassA(aTwoOrig);
        owner.setSimpleList(Arrays.asList(aOneOrig, aTwoOrig));
        when(uowMock.registerExistingObject(aOneOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aOneClone);
        when(uowMock.registerExistingObject(aTwoOrig, new CloneRegistrationDescriptor(descriptor))).thenReturn(aTwoClone);
        when(uowMock.isEntityType(OWLClassA.class)).thenReturn(true);

        final List<?> clone = (List<?>) builder.buildClone(owner, OWLClassC.getSimpleListField(), owner.getSimpleList(),
                                                           new CloneConfiguration(descriptor, false));
        assertEquals(2, clone.size());
        assertSame(aOneClone, clone.get(0));
        assertSame(aTwoClone, clone.get(1));
        verify(uowMock).registerExistingObject(aOneOrig, new CloneRegistrationDescriptor(descriptor));
        verify(uowMock).registerExistingObject(aTwoOrig, new CloneRegistrationDescriptor(descriptor));
    }

    @Test
    void mergeChangesReplacesEmptySetWithDefaultSet() throws Exception {
        final OWLClassM target = new OWLClassM();
        target.initializeTestValues(true);
        builder.mergeChanges(OWLClassM.getIntegerSetField(), target, new HashSet<>(), Collections.emptySet());
        assertThat(target.getIntegerSet(),
                   instanceOf(CollectionFactory.createDefaultCollection(CollectionType.SET).getClass()));
    }
}
