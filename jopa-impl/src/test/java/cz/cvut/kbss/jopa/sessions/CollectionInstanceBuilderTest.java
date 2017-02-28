package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
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
}