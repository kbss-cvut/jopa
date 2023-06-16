/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SparqlQueryFactoryTest {

    private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z. }";
    private static final String SOQL_QUERY = "SELECT a FROM OWLClassA a";
    private static final Class<OWLClassA> CLS = OWLClassA.class;

    @Mock
    private UnitOfWorkImpl uowMock;

    @Mock
    private NamedQueryManager namedQueryManagerMock;

    @Mock
    private ConnectionWrapper connectionMock;

    private SparqlQueryFactory factory;

    @BeforeEach
    public void setUp() throws Exception {
        when(uowMock.getNamedQueryManager()).thenReturn(namedQueryManagerMock);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        new MetamodelMocks().setMocks(metamodel);
        when(uowMock.getMetamodel()).thenReturn(metamodel);
        this.factory = new SparqlQueryFactory(uowMock, connectionMock);
    }

    @Test
    public void testCreateNativeQuery() {
        assertNotNull(factory.createNativeQuery(QUERY));
    }

    @Test
    public void testCreateNativeQueryNull() {
        assertThrows(NullPointerException.class, () -> factory.createNativeQuery(null));
    }

    @Test
    public void testCreateNativeQueryTyped() {
        assertNotNull(factory.createNativeQuery(QUERY, CLS));
    }

    @Test
    public void testCreateNativeQueryTypedNullQuery() {
        assertThrows(NullPointerException.class, () -> factory.createNativeQuery(null, CLS));
    }

    @Test
    public void testCreateNativeQueryTypedNullType() {
        assertThrows(NullPointerException.class, () -> factory.createNativeQuery(QUERY, (Class<OWLClassA>) null));
    }

    @Test
    public void testCreateQuery() {
        assertNotNull(factory.createQuery(SOQL_QUERY));
    }

    @Test
    public void testCreateQueryNull() {
        assertThrows(NullPointerException.class, () -> factory.createQuery(null));
    }

    @Test
    public void testCreateQueryTyped() {
        assertNotNull(factory.createQuery(SOQL_QUERY, CLS));
    }

    @Test
    public void testCreateQueryTypedNullQuery() {
        assertThrows(NullPointerException.class, () -> factory.createQuery(null, CLS));
    }

    @Test
    public void testCreateQueryTypedNullType() {
        assertThrows(NullPointerException.class, () -> factory.createQuery(SOQL_QUERY, null));
    }

    @Test
    public void createQueryWithMappingPassesCorrectMapperToQueryInstance() {
        final String mapping = "testMapping";
        final SparqlResultMapper mapperMock = mock(SparqlResultMapper.class);
        final ResultSetMappingManager managerMock = mock(ResultSetMappingManager.class);
        when(uowMock.getResultSetMappingManager()).thenReturn(managerMock);
        when(managerMock.getMapper(mapping)).thenReturn(mapperMock);

        final Query q = factory.createNativeQuery(QUERY, mapping);
        assertNotNull(q);
        verify(managerMock).getMapper(mapping);
    }

    @Test
    public void createNamedQueryRetrievesNamedQueryFromManagerAndReturnsCorrespondingNativeQuery() {
        final String queryName = "testQuery";
        when(namedQueryManagerMock.getQuery(queryName)).thenReturn(QUERY);
        final Query q = factory.createNamedQuery(queryName);
        assertNotNull(q);
        verify(namedQueryManagerMock).getQuery(queryName);
    }

    @Test
    public void createNamedTypedQueryRetrievesNamedQueryFromManagerAndReturnsCorrespondingNativeQuery() {
        final String queryName = "testQuery";
        when(namedQueryManagerMock.getQuery(queryName)).thenReturn(QUERY);
        final TypedQuery<OWLClassA> q = factory.createNamedQuery(queryName, OWLClassA.class);
        assertNotNull(q);
        verify(namedQueryManagerMock).getQuery(queryName);
    }
}
