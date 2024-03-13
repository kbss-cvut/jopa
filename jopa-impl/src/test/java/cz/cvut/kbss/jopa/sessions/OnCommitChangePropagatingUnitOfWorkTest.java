package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelFactory;
import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OnCommitChangePropagatingUnitOfWorkTest extends UnitOfWorkTestBase {

    @BeforeAll
    static void setUpBeforeAll() {
        MetamodelFactory.setInstantiableTypeGenerator(new PersistenceContextAwareClassGenerator() {
            @Override
            public <T> Class<? extends T> generate(Class<T> entityClass) {
                return entityClass;
            }
        });
    }

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        this.uow = new OnCommitChangePropagatingUnitOfWork(serverSessionStub, new Configuration());
        uow.begin();
    }

    @AfterAll
    static void tearDownAfterAll() {
        MetamodelFactory.reset();
    }

    @Test
    void commitToStorageCalculatesChangesToExistingObjectsAndPropagatesThemToStorage() {
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        clone.setStringAttribute("new string value");
        assertTrue(uow.uowChangeSet.getExistingObjectsChanges().isEmpty());
        uow.commit();
        verify(storageMock).merge(clone, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void commitToStorageDeletesRemovedObjectsFromStorage() {
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(clone);
        verify(storageMock, never()).remove(clone.getUri(), OWLClassA.class, descriptor);
        uow.commit();
        verify(storageMock).remove(clone.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void removeObjectRegistersObjectForDeletion() {
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(clone);
        verify(storageMock, never()).remove(clone.getUri(), OWLClassA.class, descriptor);
        assertTrue(uow.deletedObjects.containsKey(clone));
    }

    @Test
    void mergeDetachedRegistersChangesToSpecifiedObjectAndReturnsManagedClone() {
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(entityA);
        final OWLClassA toMerge = new OWLClassA(entityA.getUri());
        toMerge.setStringAttribute("Different string");
        toMerge.setTypes(Generators.generateTypes(2));

        final OWLClassA result = uow.mergeDetached(toMerge, descriptor);
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(toMerge.getStringAttribute(), result.getStringAttribute());
        assertEquals(toMerge.getTypes(), result.getTypes());
        assertTrue(uow.contains(result));
        assertTrue(uow.uowChangeSet.hasChanges());
        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(uow.getOriginal(result));
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().stringAttribute())));
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().typesSpec())));
        verify(storageMock, never()).merge(any(), any(), any());
    }

    @Test
    void commitThrowsAttributeModificationForbiddenExceptionWhenChangeConcernsLexicalValueAttribute() {
        final OWLClassM original = new OWLClassM();
        original.initializeTestValues(true);
        final OWLClassM clone = (OWLClassM) uow.registerExistingObject(original, descriptor);
        clone.setLexicalForm("Cannot change");
        assertThrows(AttributeModificationForbiddenException.class, () -> uow.commit());
    }
}
