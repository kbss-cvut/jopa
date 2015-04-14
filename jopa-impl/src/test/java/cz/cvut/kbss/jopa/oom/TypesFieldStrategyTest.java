package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class TypesFieldStrategyTest {

    private static URI PK = URI.create("http;//krizik.felk.cvut.cz/ontologies/jopa#entityA");

    @Mock
    private EntityType<OWLClassA> etAMock;
    @Mock
    private Attribute strAttMock;
    @Mock
    private TypesSpecification typesMock;
    @Mock
    private Identifier idAMock;
    @Mock
    private EntityMappingHelper mapperMock;

    private TypesFieldStrategy<OWLClassA> strategy;
    private AxiomValueGatherer gatherer;

    private OWLClassA entityA;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        TestEnvironmentUtils.initOWLClassAMocks(etAMock, strAttMock, typesMock, idAMock);

        this.entityA = new OWLClassA();
        entityA.setUri(PK);
        EntityDescriptor descriptor = new EntityDescriptor();

        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        this.strategy = new TypesFieldStrategy<>(etAMock, typesMock, descriptor.getAttributeDescriptor(typesMock), mapperMock);
        gatherer.addValue(Assertion.createClassAssertion(false), new Value<>(PK), null);
    }

    @Test
    public void extractsTypesForPersist() throws Exception {
        final int count = 5;
        entityA.setTypes(TestEnvironmentUtils.generateTypes(count));
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(count, toAdd.size());
        verifyCollectionsAreEqual(entityA.getTypes(), toAdd);
        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
    }

    @Test
    public void extractsAddedTypes() throws Exception {
        final int count = 5;
        entityA.setTypes(TestEnvironmentUtils.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> addedTypes = new HashSet<>();
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedOne");
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedTwo");
        entityA.getTypes().addAll(addedTypes);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(addedTypes.size(), toAdd.size());
        verifyCollectionsAreEqual(addedTypes, toAdd);
        assertTrue(OOMTestUtils.getTypesToRemove(gatherer).isEmpty());
    }

    private void verifyCollectionsAreEqual(Set<String> expected, Set<URI> actual) {
        for (URI type : actual) {
            assertTrue(expected.contains(type.toString()));
        }
    }

    private OWLClassA createOriginal() {
        final OWLClassA a = new OWLClassA();
        a.setUri(PK);
        if (entityA.getTypes() != null) {
            a.setTypes(new HashSet<>(entityA.getTypes()));
        }
        a.setStringAttribute(a.getStringAttribute());
        return a;
    }

    @Test
    public void extractsRemovedTypes() throws Exception {
        final int count = 5;
        entityA.setTypes(TestEnvironmentUtils.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> removedTypes = new HashSet<>();
        int i = 0;
        final Iterator<String> it = entityA.getTypes().iterator();
        while (it.hasNext()) {
            if (i % 2 != 0) {
                removedTypes.add(it.next());
                it.remove();
            } else {
                it.next();
            }
            i++;
        }
        assertFalse(removedTypes.isEmpty());
        assertFalse(entityA.getTypes().isEmpty());

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(removedTypes, toRemove);
        assertTrue(OOMTestUtils.getTypesToAdd(gatherer).isEmpty());
    }

    @Test
    public void extractsRemovedTypesWhenValueIsNull() throws Exception {
        final int count = 5;
        entityA.setTypes(TestEnvironmentUtils.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        entityA.setTypes(null);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(original.getTypes(), toRemove);
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }

    @Test
    public void extractsAddedAndRemovedTypes() throws Exception {
        final int count = 5;
        entityA.setTypes(TestEnvironmentUtils.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> removedTypes = new HashSet<>();
        int i = 0;
        final Iterator<String> it = entityA.getTypes().iterator();
        while (it.hasNext()) {
            if (i % 2 != 0) {
                removedTypes.add(it.next());
                it.remove();
            } else {
                it.next();
            }
            i++;
        }
        final Set<String> addedTypes = new HashSet<>();
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedOne");
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedTwo");
        entityA.getTypes().addAll(addedTypes);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(removedTypes, toRemove);
        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(addedTypes.size(), toAdd.size());
        verifyCollectionsAreEqual(addedTypes, toAdd);
    }

    @Test
    public void extractsNothingWhenThereAreNoTypesAndNoOriginal() throws Exception {
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(null);
        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }

    @Test
    public void extractsNothingWhenOriginalTypesAndCurrentTypesAreNull() throws Exception {
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }
}