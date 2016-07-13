package cz.cvut.kbss.jopa.query;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

public class NamedQueryManagerTest {

    private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private NamedQueryManager queryManager;

    @Before
    public void setUp() {
        this.queryManager = new NamedQueryManager();
    }

    @Test
    public void addedQueryCanBeRetrieved() {
        final String name = "selectAll";
        queryManager.addNamedQuery(name, QUERY);
        final String res = queryManager.getQuery(name);
        assertEquals(QUERY, res);
    }

    @Test
    public void addingQueryWithExistingNameThrowsIllegalArgument() {
        final String name = "selectAll";
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Query with name " + name + " already exists in this persistence unit.");
        queryManager.addNamedQuery(name, QUERY);
        queryManager.addNamedQuery(name, "TEST");
    }

    @Test
    public void retrievingUnknownQueryThrowsIllegalArgument() {
        final String name = "selectAll";
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Query with name " + name + " was not found in this persistence unit.");
        queryManager.getQuery(name);
    }
}