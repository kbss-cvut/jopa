/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class FactoryOfFactoriesTest {

    @Mock
    private Repository repository;

    @Mock

    private RepositoryConnection connection;

    @Test
    void isRepositoryGraphDBChecksForPresenceOfInternalGraphDBEntityIds() throws Exception {
        when(repository.getConnection()).thenReturn(connection);
        final ValueFactory vf = SimpleValueFactory.getInstance();
        when(connection.getValueFactory()).thenReturn(vf);
        final BooleanQuery query = mock(BooleanQuery.class);
        when(connection.prepareBooleanQuery(anyString())).thenReturn(query);
        when(query.evaluate()).thenReturn(true);

        assertTrue(Rdf4jFactoryOfFactories.isRepositoryGraphDB(repository));
        verify(query).setBinding(anyString(), eq(vf.createIRI(Rdf4jFactoryOfFactories.GRAPHDB_INTERNAL_ID_PROPERTY)));
    }

}
