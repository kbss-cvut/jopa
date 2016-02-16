/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.sail.memory.MemoryStore;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class SesamePropertiesTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa#Entity");
    private static final Assertion ASSERTION =
            Assertion.createDataPropertyAssertion(java.net.URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#assertionOne"), false);
    private static final String LANG = "en";

    @Mock
    private Connector connectorMock;
    @Mock
    private SesameConnection connectionMock;

    private URI subject;

    private ValueFactory vf;
    private MemoryStore store;

    private Properties properties;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.store = new MemoryStore();
        store.initialize();
        this.vf = store.getValueFactory();
        this.subject = vf.createURI(SUBJECT.getIdentifier().toString());
        final SesameAdapter adapterMock = mock(SesameAdapter.class);
        when(adapterMock.getConnector()).thenReturn(connectorMock);
        when(adapterMock.getValueFactory()).thenReturn(vf);
        when(adapterMock.getLanguage()).thenReturn(LANG);
        this.properties = new SesameProperties(connectionMock, adapterMock);
    }

    @After
    public void tearDown() throws Exception {
        store.shutDown();
    }

    @Test
    public void testGetProperties() throws Exception {
        final Collection<Statement> statements = statementsForProperties(initProperties());
        when(connectorMock.findStatements(subject, null, null, false)).thenReturn(statements);

        final Collection<Axiom<?>> result = properties.getProperties(SUBJECT, null, false);
        assertEquals(statements.size(), result.size());
        for (Axiom<?> ax : result) {
            assertEquals(SUBJECT, ax.getSubject());
            final Statement stmt = vf.createStatement(subject, vf.createURI(ax.getAssertion().getIdentifier().toString()),
                    SesameUtils.createDataPropertyLiteral(ax.getValue().getValue(), LANG, vf));
            assertTrue(statements.contains(stmt));
        }
    }

    @Test
    public void testAddProperties() throws Exception {
        final Map<Assertion, Set<Value<?>>> toAdd = initProperties();
        final Collection<Statement> statements = statementsForProperties(toAdd);

        properties.addProperties(SUBJECT, null, toAdd);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);

        verify(connectorMock).addStatements(captor.capture());
        final Collection<?> added = captor.getValue();
        assertEquals(statements.size(), added.size());
        assertTrue(statements.containsAll(added));
    }

    private Map<Assertion, Set<Value<?>>> initProperties() {
        final Map<Assertion, Set<Value<?>>> map = new HashMap<>();
        final Set<Value<?>> values = new HashSet<>();
        for (int i = 0; i < 5; i++) {
            values.add(new Value<>("value" + i));
        }
        map.put(ASSERTION, values);
        return map;
    }

    private Collection<Statement> statementsForProperties(Map<Assertion, Set<Value<?>>> properties) {
        final Collection<Statement> stmts = new HashSet<>();
        for (Assertion a : properties.keySet()) {
            final URI property = vf.createURI(a.getIdentifier().toString());
            for (Value<?> v : properties.get(a)) {
                stmts.add(vf.createStatement(subject, property, SesameUtils.createDataPropertyLiteral(v.getValue(), LANG, vf)));
            }
        }
        return stmts;
    }

    @Test
    public void testRemoveProperties() throws Exception {
        final Map<Assertion, Set<Value<?>>> toRemove = initProperties();
        final Collection<Statement> statements = statementsForProperties(toRemove);

        properties.removeProperties(SUBJECT, null, toRemove);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);

        verify(connectorMock).removeStatements(captor.capture());
        final Collection<?> added = captor.getValue();
        assertEquals(statements.size(), added.size());
        assertTrue(statements.containsAll(added));
    }
}