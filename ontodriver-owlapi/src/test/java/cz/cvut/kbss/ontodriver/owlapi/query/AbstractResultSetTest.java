/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.Variable;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AbstractResultSetTest {

    private static final String ASK_QUERY = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/TypeA> . }";
    private static final String SELECT_QUERY = "SELECT DISTINCT ?x WHERE { ?x ?y ?z . }";

    @Mock
    private OwlapiStatement statement;

    @Test
    void createResultSetReturnAskResultSetForAskQuery() {
        final QueryResult<OWLObject> qr = mock(QueryResult.class);
        final ResultSet result = AbstractResultSet.createResultSet(qr, statement, ASK_QUERY);
        assertTrue(result instanceof AskResultSet);
    }

    @Test
    void createResultSetReturnSelectResultSetForSelectQuery() {
        final QueryResult<OWLObject> qr = initSelectResult();
        final ResultSet result = AbstractResultSet.createResultSet(qr, statement, SELECT_QUERY);
        assertTrue(result instanceof SelectResultSet);
    }

    private QueryResult<OWLObject> initSelectResult() {
        final QueryResult<OWLObject> qr = mock(QueryResult.class);
        final Variable<OWLObject> v = mock(Variable.class);
        when(v.getName()).thenReturn("x");
        when(qr.getResultVars()).thenReturn(Collections.singletonList(v));
        return qr;
    }
}