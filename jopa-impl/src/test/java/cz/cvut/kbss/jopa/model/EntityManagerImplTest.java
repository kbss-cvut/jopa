/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.ServerSessionStub;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.sessions.cache.DisabledCacheManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class EntityManagerImplTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private EntityManagerFactoryImpl emfMock;

    @Mock
    private ConnectionWrapper connectorMock;

    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodelMock;

    private MetamodelMocks mocks;

    private EntityManagerImpl em;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final ServerSessionStub serverSessionMock = spy(new ServerSessionStub(connectorMock));
        when(serverSessionMock.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionMock.getLiveObjectCache()).thenReturn(new DisabledCacheManager());
        this.uow = spy(new UnitOfWorkImpl(serverSessionMock));
        doReturn(uow).when(serverSessionMock).acquireUnitOfWork();
        when(emfMock.getMetamodel()).thenReturn(metamodelMock);
        this.mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.em = new EntityManagerImpl(emfMock, new Configuration(Collections.emptyMap()), serverSessionMock);
    }

    @Test
    public void testCascadeMergeOnNullCollection() throws Exception {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityJ"));
        mocks.forOwlClassJ().setAttribute().getJavaField().setAccessible(true);
        assertNull(j.getOwlClassA());

        em.merge(j);
        final ArgumentCaptor<OWLClassJ> argumentCaptor = ArgumentCaptor.forClass(OWLClassJ.class);
        verify(uow).mergeDetached(argumentCaptor.capture(), any(Descriptor.class));
        assertSame(j, argumentCaptor.getValue());
        // Check that there is no exception thrown (there was a NPX bug in merging null collections) and that
        // the merged object is correctly passed to merge in UoW
    }

    @Test
    public void mergeDetachedWithSingletonSet() throws Exception {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityF"));
        final OWLClassA a = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA"));
        j.setOwlClassA(Collections.singleton(a));

        final OWLClassJ merged = em.merge(j);
        assertSame(j, merged);
        verify(uow).mergeDetached(eq(j), any());
        verify(uow).mergeDetached(eq(a), any());
    }

    @Test
    public void mergeDetachedWithSingletonList() throws Exception {
        final OWLClassC c = new OWLClassC(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityF"));
        final OWLClassA a = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA"));
        c.setSimpleList(Collections.singletonList(a));
        // Just for this test
        when(mocks.forOwlClassC().simpleListAtt().getCascadeTypes()).thenReturn(new CascadeType[]{CascadeType.MERGE});

        final OWLClassC merged = em.merge(c);
        assertSame(c, merged);
        verify(uow).mergeDetached(eq(c), any());
        verify(uow).mergeDetached(eq(a), any());
    }

    @Test
    public void unwrapReturnsItselfWhenClassMatches() throws Exception {
        assertSame(em, em.unwrap(EntityManagerImpl.class));
    }

    @Test
    public void containsThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        final UnknownEntity obj = new UnknownEntity();
        em.contains(obj);
    }

    @Test
    public void findThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        em.find(UnknownEntity.class, "primaryKey", new EntityDescriptor());
    }

    @Test
    public void persistThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        em.persist(new UnknownEntity(), new EntityDescriptor());
    }

    @Test
    public void mergeThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        em.merge(new UnknownEntity(), new EntityDescriptor());
    }

    @Test
    public void removeThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        em.remove(new UnknownEntity());
    }

    @Test
    public void refreshThrowsIllegalArgumentForNonEntity() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.");
        em.refresh(new UnknownEntity());
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#UnknownEntity")
    private static class UnknownEntity {
        @Id
        private URI id;

        public URI getId() {
            return id;
        }

        public void setId(URI id) {
            this.id = id;
        }
    }

    @Test
    public void closeNotifiesEntityManagerFactoryOfClosing() {
        assertTrue(em.isOpen());
        em.close();
        assertFalse(em.isOpen());
        verify(emfMock).entityManagerClosed(em);
    }
}
