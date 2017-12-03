/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TypesHandlerTest {

    private static final String STR_PK = "http://krizik.felk.cvut.cz/ontologies/jopa/entityX";
    private static final String TYPE_BASE = "http://krizik.felk.cvut.cz/ontologies/jopa/type";
    private static final NamedResource NAMED_PK = NamedResource.create(STR_PK);

    private static ValueFactory vf;

    @Mock
    private Connector connectorMock;

    private TypesHandler handler;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        vf = SimpleValueFactory.getInstance();
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.handler = new TypesHandler(connectorMock, vf);
    }

    @Test
    public void getsTypesWithoutContext() throws Exception {
        final Collection<Statement> statements = initStatements(null);
        when(connectorMock.findStatements(vf.createIRI(STR_PK), RDF.TYPE, null, false, null))
                .thenReturn(statements);
        final Set<Axiom<java.net.URI>> res = handler.getTypes(NamedResource.create(STR_PK), null, false);
        assertEquals(statements.size(), res.size());
        final Set<java.net.URI> uris = new HashSet<>();
        for (Axiom<java.net.URI> u : res) {
            uris.add(u.getValue().getValue());
        }
        for (Statement stmt : statements) {
            assertTrue(uris.contains(java.net.URI.create(stmt.getObject().stringValue())));
        }
    }

    private Collection<Statement> initStatements(IRI context) {
        final Collection<Statement> statements = new HashSet<>();
        final IRI subject = vf.createIRI(STR_PK);
        for (int i = 0; i < 6; i++) {
            final Statement stmt = vf.createStatement(subject, RDF.TYPE, vf.createIRI(TYPE_BASE + i), context);
            statements.add(stmt);
        }
        return statements;
    }

    @Test
    public void getsTypesIncludingInferredAndInContext() throws Exception {
        final IRI context = vf.createIRI("http://krizik.felk.cvut.cz/ontologies/contextOne");
        final Collection<Statement> statements = initStatements(context);
        when(connectorMock.findStatements(vf.createIRI(STR_PK), RDF.TYPE, null, true, context)).thenReturn(statements);
        final Set<Axiom<java.net.URI>> res = handler
                .getTypes(NAMED_PK, java.net.URI.create(context.stringValue()), true);
        assertEquals(statements.size(), res.size());
        final Set<java.net.URI> uris = new HashSet<>();
        for (Axiom<java.net.URI> u : res) {
            uris.add(u.getValue().getValue());
        }
        for (Statement stmt : statements) {
            assertTrue(uris.contains(java.net.URI.create(stmt.getObject().stringValue())));
        }
        verify(connectorMock).findStatements(vf.createIRI(STR_PK), RDF.TYPE, null, true, context);
    }

    @Test
    public void getsEmptyTypes() throws Exception {
        when(connectorMock.findStatements(vf.createIRI(STR_PK), RDF.TYPE, null, false))
                .thenReturn(Collections.emptySet());
        final Set<Axiom<java.net.URI>> res = handler.getTypes(NamedResource.create(STR_PK), null, true);
        assertTrue(res.isEmpty());
    }

    @Test
    public void testPersistTypes() throws Exception {
        final Set<java.net.URI> types = initTypes();
        handler.addTypes(NAMED_PK, null, types);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).addStatements(captor.capture());
        final Collection<?> statements = captor.getValue();
        assertEquals(types.size(), statements.size());
        for (Object o : statements) {
            final Statement stmt = (Statement) o;
            assertEquals(STR_PK, stmt.getSubject().stringValue());
            assertTrue(types.contains(java.net.URI.create(stmt.getObject().stringValue())));
        }
    }

    private Set<java.net.URI> initTypes() {
        final Set<java.net.URI> types = new HashSet<>();
        for (int i = 0; i < 10; i++) {
            types.add(java.net.URI.create(TYPE_BASE + i));
        }
        return types;
    }

    @Test
    public void testRemoveTypes() throws Exception {
        final Set<java.net.URI> types = initTypes();
        handler.removeTypes(NAMED_PK, null, types);
        final ArgumentCaptor<Collection> captor = ArgumentCaptor.forClass(Collection.class);
        verify(connectorMock).removeStatements(captor.capture());
        final Collection<?> statements = captor.getValue();
        assertEquals(types.size(), statements.size());
        for (Object o : statements) {
            final Statement stmt = (Statement) o;
            assertEquals(STR_PK, stmt.getSubject().stringValue());
            assertTrue(types.contains(java.net.URI.create(stmt.getObject().stringValue())));
        }
    }
}