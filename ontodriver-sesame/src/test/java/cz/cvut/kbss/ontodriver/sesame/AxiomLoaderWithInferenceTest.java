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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.environment.TestRepositoryProvider;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;

class AxiomLoaderWithInferenceTest {

    private final TestRepositoryProvider repositoryProvider = new TestRepositoryProvider();

    private final ValueFactory vf = SimpleValueFactory.getInstance();

    private Connector connector;

    private AxiomLoader sut;

    @BeforeEach
    void setUp() throws Exception {
        this.connector = repositoryProvider.createConnector(true);
        final DriverConfiguration driverConfig = new DriverConfiguration(TestRepositoryProvider.storageProperties());

        this.sut = new AxiomLoader(connector, vf, new RuntimeConfiguration(driverConfig));
    }

    @AfterEach
    void tearDown() throws Exception {
        connector.close();
        repositoryProvider.close();
    }

    /**
     * Note that this behavior works for RDF4J, but does not work for GraphDB, which stores inferred statements in a
     * separate context, so that they are accessible only from it or from the default context.
     */
    @Test
    void findStatementsLoadsInferredStatementsEvenWhenContextIsSpecifiedForAssertion() throws Exception {
        final IRI childType = vf.createIRI(Generator.generateUri().toString());
        final IRI parentType = vf.createIRI(Generator.generateUri().toString());
        final IRI instance = vf.createIRI(Generator.generateUri().toString());
        final URI context = Generator.generateUri();
        try (final RepositoryConnection conn = connector.unwrap(Repository.class).getConnection()) {
            conn.begin();
            conn.add(childType, RDFS.SUBCLASSOF, parentType, vf.createIRI(context.toString()));
            conn.add(instance, RDF.TYPE, childType, vf.createIRI(context.toString()));
            conn.commit();
        }

        final AxiomDescriptor desc = new AxiomDescriptor(NamedResource.create(instance.toString()));
        final Assertion a = Assertion.createClassAssertion(true);
        desc.addAssertion(a);
        desc.addAssertionContext(a, context);
        connector.begin();
        final Collection<Axiom<?>> result = sut.loadAxioms(desc);
        assertFalse(result.isEmpty());
        assertTrue(result.stream().anyMatch(ax -> ax.getValue().stringValue().equals(parentType.toString())));
    }
}
