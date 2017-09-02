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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.IntStream;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class CollectionInstanceBuilderTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private UnitOfWorkImpl uowMock;

    private Descriptor descriptor;

    private CollectionInstanceBuilder builder;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        CloneBuilderImpl cloneBuilder = new CloneBuilderImpl(uowMock);
        this.builder = new CollectionInstanceBuilder(cloneBuilder, uowMock);
        when(uowMock.createIndirectCollection(any(), any(), any(Field.class))).thenAnswer(invocation -> {
            final Collection<?> col = (Collection<?>) invocation.getArguments()[0];
            final Object owner = invocation.getArguments()[1];
            final Field field = (Field) invocation.getArguments()[2];
            if (col instanceof List) {
                return new IndirectList<>(owner, field, uowMock, (List<?>) col);
            } else {
                return new IndirectSet<>(owner, field, uowMock, (Set<?>) col);
            }
        });
        this.descriptor = new EntityDescriptor();
    }

    @Test
    public void buildCloneCreatesDefaultListWhenUnknownListImplementationIsPassedAsArgument() throws Exception {
        final CollectionOwner owner = new CollectionOwner();
        owner.list = new TestList<>(owner);
        IntStream.range(0, 10).forEach(i -> owner.list.add("String" + i));
        final Object result = builder.buildClone(owner, CollectionOwner.listField(), owner.list, descriptor);
        assertNotNull(result);
        assertTrue(result instanceof List);
        final List<?> lstResult = (List<?>) result;
        owner.list.forEach(e -> assertTrue(lstResult.contains(e)));
    }

    @Test
    public void buildCloneCreatesDefaultSetWhenUnknownSetImplementationIsPassedAsArgument() throws Exception {
        final CollectionOwner owner = new CollectionOwner();
        owner.set = new TestSet<>(owner);
        IntStream.range(0, 10).forEach(i -> owner.set.add("String" + i));
        final Object result = builder.buildClone(owner, CollectionOwner.setField(), owner.set, descriptor);
        assertNotNull(result);
        assertTrue(result instanceof Set);
        final Set<?> setResult = (Set<?>) result;
        owner.set.forEach(e -> assertTrue(setResult.contains(e)));
    }

    @Test
    public void buildCloneThrowsUnsupportedCollectionTypeExceptionWhenCollectionIsNeitherListNorSet() throws Exception {
        final CollectionOwner owner = new CollectionOwner();
        owner.queue = new TestQueue<>(owner);
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage("Cannot clone unsupported collection instance of type " + owner.queue.getClass() + ".");
        builder.buildClone(owner, CollectionOwner.queueField(), owner.queue, descriptor);
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
        final OWLClassJ owner = new OWLClassJ(Generators.createIndividualIdentifier());
        final OWLClassA aOrig = Generators.generateOwlClassAInstance();
        final OWLClassA aClone = new OWLClassA(aOrig);
        owner.setOwlClassA(Collections.singleton(aOrig));
        when(uowMock.registerExistingObject(aOrig, descriptor)).thenReturn(aClone);
        when(uowMock.isTypeManaged(OWLClassA.class)).thenReturn(true);
        final Set<OWLClassA> clone = (Set<OWLClassA>) builder
                .buildClone(owner, OWLClassJ.getOwlClassAField(), owner.getOwlClassA(), descriptor);
        assertEquals(owner.getOwlClassA().size(), clone.size());
        assertSame(aClone, clone.iterator().next());
        verify(uowMock).registerExistingObject(aOrig, descriptor);
    }
}