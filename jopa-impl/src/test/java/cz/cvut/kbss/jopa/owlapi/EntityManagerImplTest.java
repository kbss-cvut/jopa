package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

/**
 * @author kidney
 */
public class EntityManagerImplTest {

    @Mock
    private EntityManagerFactoryImpl emfMock;

    @Mock
    private ServerSession serverSessionMock;

    @Mock
    private UnitOfWorkImpl uowMock;

    @Mock
    private Metamodel metamodelMock;

    @Mock
    private EntityType<OWLClassJ> etJ;
    @Mock
    private Identifier idJMock;
    @Mock
    private PluralAttributeImpl clsAMock;

    private EntityManagerImpl em;


    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(serverSessionMock.acquireUnitOfWork()).thenReturn(uowMock);
        when(uowMock.getMetamodel()).thenReturn(metamodelMock);
        when(emfMock.getMetamodel()).thenReturn(metamodelMock);
        TestEnvironmentUtils.initOWLClassJMocks(etJ, clsAMock, idJMock);
        when(metamodelMock.entity(OWLClassJ.class)).thenReturn(etJ);
        this.em = new EntityManagerImpl(emfMock, Collections.emptyMap(), serverSessionMock);
    }

    @Test
    public void testCascadeMergeOnNullCollection() throws Exception {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityJ"));
        when(uowMock.getState(eq(j), any(Descriptor.class))).thenReturn(EntityManagerImpl.State.NOT_MANAGED);
        when(uowMock.mergeDetached(eq(j), any(Descriptor.class))).thenReturn(j);
        clsAMock.getJavaField().setAccessible(true);
        assertNull(j.getOwlClassA());

        em.merge(j);
        // Just check that no exception is thrown. There was a bug which caused NPX for null collections on cascade merge
    }
}