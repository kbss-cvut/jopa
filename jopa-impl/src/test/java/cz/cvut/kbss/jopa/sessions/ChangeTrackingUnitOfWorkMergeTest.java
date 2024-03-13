package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ChangeTrackingUnitOfWorkMergeTest extends UnitOfWorkMergeTestRunner {

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new ChangeTrackingUnitOfWork(serverSessionStub, new Configuration());
    }

    @Test
    void testMergeDetachedExisting() {
        final OWLClassA result = mergeDetached();
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        final ArgumentCaptor<FieldSpecification> ac = ArgumentCaptor.forClass(FieldSpecification.class);
        verify(storageMock, atLeastOnce()).merge(any(Object.class), ac.capture(), eq(descriptor));
        final List<FieldSpecification> mergedFields = ac.getAllValues();
        assertTrue(mergedFields.contains(metamodelMocks.forOwlClassA().stringAttribute()));
        assertTrue(mergedFields.contains(metamodelMocks.forOwlClassA().typesSpec()));
    }
}
