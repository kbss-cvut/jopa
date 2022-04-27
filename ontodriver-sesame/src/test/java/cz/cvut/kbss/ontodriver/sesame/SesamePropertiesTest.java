/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.loader.StatementLoader;
import cz.cvut.kbss.ontodriver.sesame.loader.StatementLoaderFactory;
import cz.cvut.kbss.ontodriver.sesame.util.AxiomBuilder;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class SesamePropertiesTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Assertion ASSERTION = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);

    @Mock
    private Connector connectorMock;

    @Mock
    private RuntimeConfiguration configMock;

    private IRI subject;

    private ValueFactory vf;

    private Properties properties;

    @BeforeEach
    void setUp() {
        this.vf = SimpleValueFactory.getInstance();
        this.subject = vf.createIRI(SUBJECT.getIdentifier().toString());
        final SesameAdapter adapterMock = mock(SesameAdapter.class);
        when(adapterMock.getConnector()).thenReturn(connectorMock);
        when(connectorMock.getValueFactory()).thenReturn(vf);
        when(adapterMock.getConfig()).thenReturn(configMock);
        this.properties = new SesameProperties(adapterMock, () -> {
        }, () -> {
        });
    }

    @Test
    void testGetProperties() throws Exception {
        final StatementLoaderFactory factory = mock(StatementLoaderFactory.class);
        when(configMock.getStatementLoaderFactory()).thenReturn(factory);
        final AxiomBuilder axiomBuilder = new AxiomBuilder(SUBJECT, Collections.emptyMap(), Assertion.createUnspecifiedPropertyAssertion(false));
        final StatementLoader loader = new StatementLoader(connectorMock, subject, axiomBuilder);
        when(factory.create(any(), any(), any())).thenReturn(loader);
        final Collection<Statement> statements = statementsForProperties(initProperties());
        when(connectorMock.findStatements(subject, null, null, false, Collections.emptySet())).thenReturn(statements);

        final Collection<Axiom<?>> result = properties.getProperties(SUBJECT, null, false);
        assertEquals(statements.size(), result.size());
        for (Axiom<?> ax : result) {
            assertEquals(SUBJECT, ax.getSubject());
            final Statement stmt = vf
                    .createStatement(subject, vf.createIRI(ax.getAssertion().getIdentifier().toString()),
                            SesameUtils
                                    .createLiteral(ax.getValue().getValue(), Constants.DEFAULT_LANG, vf));
            assertTrue(statements.contains(stmt));
        }
    }

    @Test
    void testAddProperties() throws Exception {
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
            final IRI property = vf.createIRI(a.getIdentifier().toString());
            stmts.addAll(properties.get(a).stream().map(v -> vf
                            .createStatement(subject, property,
                                    SesameUtils.createLiteral(v.getValue(), Constants.DEFAULT_LANG, vf)))
                    .collect(Collectors.toList()));
        }
        return stmts;
    }

    @Test
    void testRemoveProperties() throws Exception {
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
