package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.NoopInstantiableTypeGenerator;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelFactory;
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
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class OnCommitChangePropagatingUnitOfWorkMergeTest extends UnitOfWorkMergeTestRunner {

    @BeforeAll
    static void setUpBeforeAll() {
        MetamodelFactory.setInstantiableTypeGenerator(NoopInstantiableTypeGenerator.INSTANCE);
    }

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new OnCommitChangePropagatingUnitOfWork(serverSessionStub, new Configuration());
    }

    @AfterAll
    static void tearDownAfterAll() {
        MetamodelFactory.reset();
    }

    @Test
    void testMergeDetachedExisting() {
        final OWLClassA result = mergeDetached();
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(uow.getOriginal(result));
        assertNotNull(changeSet);
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().stringAttribute())));
    }
}
