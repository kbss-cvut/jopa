package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver_new.PreparedStatement;

public class SesamePreparedStatementTest {

	@Mock
	private StatementExecutor executorMock;
	@Mock
	private TupleQueryResult resultMock;

	private PreparedStatement statement;
	private Field paramsValuesField;
	private Field paramNamesField;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(executorMock.executeSelectQuery(any(String.class))).thenReturn(resultMock);
		this.paramsValuesField = SesamePreparedStatement.class.getDeclaredField("paramValues");
		paramsValuesField.setAccessible(true);
		this.paramNamesField = SesamePreparedStatement.class.getDeclaredField("paramNames");
		paramNamesField.setAccessible(true);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConstructorEmptyStatement() throws Exception {
		initStatement("");
		fail("This line should not have been reached.");
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testQueryWithoutParameters() throws Exception {
		// Of course this query is not valid, but PreparedStatement shouldn't
		// care about this, that is the job of the query engine
		final String query = "SELECT nothing WHERE { <http://subject> <http://property> <http://subject> . }";
		initStatement(query);
		final List<String> params = (List<String>) paramNamesField.get(statement);
		assertTrue(params.isEmpty());
	}

	@Test
	public void testQueryWithNewlines() throws Exception {
		final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' } WHERE\n"
				+ "{\n?person <http://xmlns.com/foaf/0.1/givenName> ?name\n}";
		final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' } WHERE\n"
				+ "{\n?person <http://xmlns.com/foaf/0.1/givenName> 'Bill'\n}";
		initStatement(query);
		statement.setObject("name", "'Bill'");
		statement.executeUpdate();
		verify(executorMock).executeUpdate(expected);
	}

	@Test
	public void testQueryWithDoubleQuotes() throws Exception {
		final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> \"William\" } WHERE\n"
				+ "{\n?person <http://xmlns.com/foaf/0.1/givenName> ?name\n}";
		final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> \"Bill\" }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> \"William\" } WHERE\n"
				+ "{\n?person <http://xmlns.com/foaf/0.1/givenName> \"Bill\"\n}";
		initStatement(query);
		statement.setObject("name", "\"Bill\"");
		statement.executeUpdate();
		verify(executorMock).executeUpdate(expected);
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testSetObject() throws Exception {
		final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
		initStatement(query);
		final List<String> params = (List<String>) paramNamesField.get(statement);
		assertEquals(4, params.size());
		final Map<String, String> paramValues = (Map<String, String>) paramsValuesField
				.get(statement);
		assertNull(paramValues.get("x"));
		assertNull(paramValues.get("y"));
		final String p1 = "<http://subject>";
		final String p2 = "\"object@en\"";
		statement.setObject("x", p1);
		assertEquals(p1, paramValues.get("x"));
		statement.setObject("y", p2);
		assertEquals(p2, paramValues.get("y"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetObjectUnknownBindingName() throws Exception {
		final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
		initStatement(query);
		statement.setObject("z", "someValue@en");
	}

	@Test
	public void testExecuteQuery() throws Exception {
		final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
		final String expected = "SELECT _:subject ?y WHERE { _:subject <http://property> ?y . }";
		initStatement(query);
		statement.setObject("x", "_:subject");
		statement.executeQuery();
		verify(executorMock).executeSelectQuery(expected);
	}

	@Test
	public void testExecuteUpdate() throws Exception {
		final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' }" + "WHERE"
				+ "{" + "?person <http://xmlns.com/foaf/0.1/givenName> ?name }";
		final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
				+ "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }"
				+ "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' }" + "WHERE"
				+ "{" + "?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }";
		initStatement(query);
		statement.setObject("name", "'Bill'");
		statement.executeUpdate();
		verify(executorMock).executeUpdate(expected);
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testClearParameters() throws Exception {
		final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
		initStatement(query);
		final Map<String, String> paramValues = (Map<String, String>) paramsValuesField
				.get(statement);
		final String p1 = "_:subject";
		final String p2 = "<http://object>";
		statement.setObject("x", p1);
		statement.setObject("y", p2);
		assertEquals(p1, paramValues.get("x"));
		assertEquals(p2, paramValues.get("y"));
		statement.clearParameters();
		assertNull(paramValues.get("x"));
		assertNull(paramValues.get("y"));
	}

	private void initStatement(final String query) throws OntoDriverException {
		this.statement = new SesamePreparedStatement(executorMock, query);
	}
}
