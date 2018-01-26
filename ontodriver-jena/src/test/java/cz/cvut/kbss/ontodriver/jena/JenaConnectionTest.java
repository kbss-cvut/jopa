package cz.cvut.kbss.ontodriver.jena;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class JenaConnectionTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private JenaConnection connection;

    @Before
    public void setUp() {
        this.connection = new JenaConnection();
    }

    @Test
    public void setAutoCommitThrowsIllegalStateForClosedConnection() {
        connection.close();
        expectClosedException();
        connection.setAutoCommit(false);
    }

    private void expectClosedException() {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage(containsString("connection is closed"));
    }

    @Test
    public void isAutoCommitThrowsIllegalStateForClosedConnection() {
        connection.close();
        expectClosedException();
        connection.isAutoCommit();
    }
}