package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class RefreshInstanceMergerTest {

    @Mock
    private UnitOfWorkImpl uowMock;

    private RefreshInstanceMerger merger;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.merger = new RefreshInstanceMerger(new CollectionFactory(uowMock));
    }

    @Test
    public void mergeChangesOverwritesSingularAttributeChangesWithSourceValues() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = new OWLClassA(original.getUri());
        clone.setStringAttribute("changedString");
        clone.setTypes(new HashSet<>(original.getTypes()));
        final ObjectChangeSet changeSet =
                ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        final FieldSpecification fieldSpec = mock(FieldSpecification.class);
        when(fieldSpec.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        changeSet.addChangeRecord(new ChangeRecordImpl(fieldSpec, clone.getStringAttribute()));

        merger.mergeChanges(changeSet);
        assertEquals(original.getStringAttribute(), clone.getStringAttribute());
    }

    @Test
    public void mergeChangesReplacesCollectionWithNewOneWithSourceValues() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        original.setTypes(new IndirectSet<>(original, OWLClassA.getTypesField(), uowMock, original.getTypes()));
        final OWLClassA clone = new OWLClassA(original.getUri());
        clone.setTypes(new HashSet<>(original.getTypes()));
        final ObjectChangeSet changeSet =
                ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        final TypesSpecification fieldSpec = mock(TypesSpecification.class);
        when(fieldSpec.isCollection()).thenReturn(true);
        when(fieldSpec.getJavaField()).thenReturn(OWLClassA.getTypesField());
        changeSet.addChangeRecord(new ChangeRecordImpl(fieldSpec, clone.getTypes()));

        merger.mergeChanges(changeSet);
        assertTrue(clone.getTypes() instanceof IndirectSet);
        assertEquals(original.getTypes(), clone.getTypes());
        final Field ownerField = IndirectCollection.class.getDeclaredField("owner");
        ownerField.setAccessible(true);
        assertEquals(clone, ownerField.get(clone.getTypes()));
    }

    @Test
    public void mergeChangesReplacesObjectPropertyCollectionWithSourceValues() throws Exception {
        final OWLClassC original = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassC clone = new OWLClassC(original.getUri());
        final List<OWLClassA> refList = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassAInstance()).collect(
                Collectors.toList());
        final List<OWLClassA> refListClone = new ArrayList<>(refList);
        original.setReferencedList(new IndirectList<>(original, OWLClassC.getRefListField(), uowMock, refList));
        clone.setReferencedList(new IndirectList<>(clone, OWLClassC.getRefListField(), uowMock, refListClone));
        clone.getReferencedList().add(Generators.generateOwlClassAInstance());
        final Attribute att = mock(Attribute.class);
        when(att.isCollection()).thenReturn(true);
        when(att.getJavaField()).thenReturn(OWLClassC.getRefListField());
        final ObjectChangeSet changeSet =
                ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        changeSet.addChangeRecord(new ChangeRecordImpl(att, refListClone));

        merger.mergeChanges(changeSet);
        assertEquals(refList.size(), clone.getReferencedList().size());
        assertTrue(clone.getReferencedList() instanceof IndirectList);
        final Field ownerField = IndirectCollection.class.getDeclaredField("owner");
        ownerField.setAccessible(true);
        assertEquals(clone, ownerField.get(clone.getReferencedList()));
    }
}