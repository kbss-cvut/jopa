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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class OwlapiPreparedStatementTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x ?y ?z . }";

    @Mock
    private TransactionalStatementExecutor executorMock;
    @Mock
    private StatementExecutorFactory executorFactoryMock;
    @Mock
    private OwlapiConnection connectionMock;
    @Mock
    private ResultSet resultSetMock;

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        when(executorMock.executeQuery(anyString(), any())).thenReturn(resultSetMock);
        final OwlapiPreparedStatement statement = new OwlapiPreparedStatement(executorFactoryMock, connectionMock,
                QUERY);
        statement.executeQuery();
        statement.setObject("x", "value");
        statement.executeQuery();
        verify(resultSetMock).close();
    }
}
