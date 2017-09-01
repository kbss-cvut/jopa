package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class UnitOfWorkMergeTest extends UnitOfWorkTestBase {

    @Before
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    public void testMergeDetachedExisting() throws Exception {
        mergeDetachedTest();
    }

    @SuppressWarnings("unchecked")
    private void mergeDetachedTest() throws Exception {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor))
                .thenReturn(Boolean.TRUE);
        final OWLClassA orig = new OWLClassA();
        orig.setUri(entityA.getUri());
        orig.setStringAttribute("oldStringAttribute");
        orig.setTypes(new HashSet<>(entityA.getTypes()));
        final Iterator<String> it = orig.getTypes().iterator();
        it.next();
        it.remove();
        when(storageMock.find(any())).thenReturn(orig);

        final OWLClassA res = uow.mergeDetached(entityA, descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        final ArgumentCaptor<Field> ac = ArgumentCaptor.forClass(Field.class);
        verify(storageMock, atLeastOnce()).merge(any(Object.class), ac.capture(), eq(descriptor));
        final List<Field> mergedFields = ac.getAllValues();
        assertTrue(mergedFields.contains(OWLClassA.getStrAttField()));
        assertTrue(mergedFields.contains(OWLClassA.getTypesField()));
    }

    @Test
    public void mergeDetachedEvictsInstanceFromCache() throws Exception {
        when(cacheManagerMock.contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(Boolean.TRUE);
        mergeDetachedTest();
        verify(cacheManagerMock).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    public void testMergeDetachedNew() throws Exception {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor))
                .thenReturn(Boolean.FALSE);
        final OWLClassA res = uow.mergeDetached(entityA, descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
        verify(storageMock).persist(entityA.getUri(), entityA, descriptor);
    }

    @Test
    public void mergeRegistersChangesInUoWChangeSet() throws Exception {
        final OWLClassA clone = new OWLClassA();
        clone.setUri(entityA.getUri());
        // These two attributes will be changed
        clone.setStringAttribute("changedStringAttribute");
        clone.setTypes(Collections.emptySet());
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(entityA);
        uow.mergeDetached(clone, descriptor);

        assertTrue(uow.hasChanges());
        final UnitOfWorkChangeSet changeSet = uow.getUowChangeSet();
        final ObjectChangeSet objectChanges = changeSet.getExistingObjectChanges(entityA);
        assertNotNull(objectChanges);
        assertEquals(2, objectChanges.getChanges().size());
        final String strAttName = OWLClassA.getStrAttField().getName();
        final Optional<ChangeRecord> rOne = objectChanges.getChanges().stream()
                                                         .filter(ch -> ch.getAttribute().getName().equals(strAttName))
                                                         .findAny();
        assertTrue(rOne.isPresent());
        assertEquals(clone.getStringAttribute(), rOne.get().getNewValue());
        final String typesAttName = OWLClassA.getTypesField().getName();
        final Optional<ChangeRecord> rTwo = objectChanges.getChanges().stream()
                                                         .filter(ch -> ch.getAttribute().getName().equals(typesAttName))
                                                         .findAny();
        assertTrue(rTwo.isPresent());
        assertEquals(clone.getTypes(), rTwo.get().getNewValue());
    }

    @Test
    public void mergeReturnsInstanceWithReferencesWithOriginalValues() {
        final OWLClassA aOriginal = new OWLClassA(entityA.getUri());
        aOriginal.setStringAttribute(entityA.getStringAttribute());
        aOriginal.setTypes(new HashSet<>(entityA.getTypes()));
        final OWLClassD dOriginal = new OWLClassD(entityD.getUri());
        dOriginal.setOwlClassA(aOriginal);
        entityA.setStringAttribute("differentString");
        entityA.getTypes().add(Vocabulary.CLASS_BASE + "addedType");
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        final LoadingParameters<OWLClassD> dParams = new LoadingParameters<>(OWLClassD.class, dOriginal.getUri(),
                descriptor, true);
        dParams.bypassCache();
        when(storageMock.find(dParams)).thenReturn(dOriginal);

        final OWLClassD result = uow.mergeDetached(entityD, descriptor);
        assertEquals(aOriginal.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(aOriginal.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    public void mergeReturnsInstanceWithUpdatedReferenceWhenItWasChangedInTheDetachedObject() {
        final OWLClassA aOriginal = Generators.generateOwlClassAInstance();
        final OWLClassD dOriginal = new OWLClassD(entityD.getUri());
        dOriginal.setOwlClassA(aOriginal);
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        final LoadingParameters<OWLClassD> dParams = new LoadingParameters<>(OWLClassD.class, dOriginal.getUri(),
                descriptor, true);
        dParams.bypassCache();
        when(storageMock.find(dParams)).thenReturn(dOriginal);

        final OWLClassD result = uow.mergeDetached(entityD, descriptor);
        assertEquals(entityA.getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    public void mergeMergesChangesIntoExistingManagedInstanceAndReturnsIt() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final OWLClassA detached = new OWLClassA(managed.getUri());
        detached.setTypes(new HashSet<>(managed.getTypes()));
        final String detachedString = "detachedStringAttribute";
        detached.setStringAttribute(detachedString);
        when(storageMock.contains(managed.getUri(), OWLClassA.class, descriptor)).thenReturn(true);

        final OWLClassA result = uow.mergeDetached(detached, descriptor);
        assertSame(managed, result);
        assertEquals(detachedString, result.getStringAttribute());
    }

    @Test
    public void mergeDoesNotAddChangeSetToUoWChangeSetWhenItContainsNoChanges() throws Exception {
        final OWLClassD managed = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final OWLClassA a2 = new OWLClassA(Generators.createIndividualIdentifier());
        a2.setStringAttribute("a2");
        final OWLClassA a2Clone = (OWLClassA) uow.registerExistingObject(a2, descriptor);
        managed.setOwlClassA(a2Clone);
        when(transactionMock.isActive()).thenReturn(true);
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        uow.attributeChanged(managed, OWLClassD.getOwlClassAField());
        assertTrue(uow.getUowChangeSet().hasChanges());
        final ObjectChangeSet originalChangeSet = uow.getUowChangeSet().getExistingObjectChanges(entityD);
        assertTrue(originalChangeSet.hasChanges());
        final OWLClassD detached = new OWLClassD(managed.getUri());
        detached.setOwlClassA(a2Clone);

        uow.mergeDetached(detached, descriptor);
        assertTrue(uow.getUowChangeSet().hasChanges());
        final ObjectChangeSet result = uow.getUowChangeSet().getExistingObjectChanges(entityD);
        assertEquals(originalChangeSet, result);
        assertTrue(result.hasChanges());
    }
}
