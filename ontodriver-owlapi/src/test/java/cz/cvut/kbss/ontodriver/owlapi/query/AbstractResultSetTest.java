package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.Variable;
import org.junit.Test;
import org.mockito.Mock;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Collections;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AbstractResultSetTest {

    private static final String ASK_QUERY = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/TypeA> . }";
    private static final String SELECT_QUERY = "SELECT DISTINCT ?x WHERE { ?x ?y ?z . }";

    @Mock
    private OwlapiStatement statement;

    @Test
    public void createResultSetReturnAskResultSetForAskQuery() throws Exception {
        final QueryResult<OWLObject> qr = mock(QueryResult.class);
        final ResultSet result = AbstractResultSet.createResultSet(qr, statement, ASK_QUERY);
        assertTrue(result instanceof AskResultSet);
    }

    @Test
    public void createResultSetReturnSelectResultSetForSelectQuery() throws Exception {
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