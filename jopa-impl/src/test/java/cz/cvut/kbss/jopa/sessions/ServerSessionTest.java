/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.accessors.DataSourceStub;
import cz.cvut.kbss.jopa.accessors.StorageAccessor;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.*;

public class ServerSessionTest {

    @Mock
    private MetamodelImpl metamodelMock;

    private ServerSession session;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        OntologyStorageProperties storageProperties = OntologyStorageProperties.ontologyUri(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa")).physicalUri(
                URI.create("file://tmp/jopa")).driver(DataSourceStub.class.getCanonicalName()).build();
        when(metamodelMock.getEntities()).thenReturn(Collections.emptySet());
        this.session = new ServerSession(storageProperties, new Configuration(Collections.emptyMap()), metamodelMock);
    }

    @Test
    public void testClose() {
        final EntityTransaction et = mock(EntityTransaction.class);
        when(et.isActive()).thenReturn(Boolean.TRUE);
        when(et.isRollbackOnly()).thenReturn(Boolean.FALSE);
        final AbstractEntityManager em = mock(AbstractEntityManager.class);
        session.transactionStarted(et, em);

        session.close();
        verify(et).setRollbackOnly();
    }

    @Test
    public void unwrapServerSessionReturnsItself() {
        assertSame(session, session.unwrap(ServerSession.class));
    }

    @Test
    public void unwrapCallThroughToStorageAccessorIfClassDoesNotMatch() {
        final StorageAccessor sa = session.unwrap(StorageAccessor.class);
        assertNotNull(sa);
    }
}
