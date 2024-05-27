/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryResult;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class Rdf4jAdapterWithStoreTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    private Repository repo;
    private ValueFactory vf;

    private ConnectionFactory factory;
    private RepoConnection connector;
    private Rdf4jAdapter adapter;

    @BeforeEach
    public void setUp() throws Exception {
        final OntologyStorageProperties sp = OntologyStorageProperties.driver(Rdf4jDataSource.class.getName())
                                                                      .physicalUri("memory-store").build();
        final DriverConfiguration configuration = new DriverConfiguration(sp);
        configuration.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.toString(true));
        final StorageConnector connectorInitializer = new StorageConnector(configuration);
        connectorInitializer.initializeRepository();
        this.factory = new ConnectionFactoryImpl(connectorInitializer);
        this.connector = factory.createStorageConnection();
        this.adapter = new Rdf4jAdapter(connector, new RuntimeConfiguration(configuration));
        this.repo = adapter.unwrap(Repository.class);
        this.vf = repo.getValueFactory();
    }

    @AfterEach
    public void tearDown() throws Exception {
        adapter.close();
        connector.close();
        factory.close();
    }

    @Test
    public void persistIndividualInTwoClassesInIndependentTransactionsIsPossible() throws Exception {
        final AxiomValueDescriptor dOne = new AxiomValueDescriptor(SUBJECT);
        final URI tOne = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA");
        dOne.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(tOne));
        final String pOne = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute";
        dOne.addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(pOne), false),
                new Value<>("StringValue"));
        adapter.persist(dOne);
        adapter.commit();
        final AxiomValueDescriptor dTwo = new AxiomValueDescriptor(SUBJECT);
        final URI tTwo = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB");
        dTwo.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(tTwo));
        final String pTwo = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute";
        dTwo.addAssertionValue(Assertion.createDataPropertyAssertion(URI.create(pTwo), false),
                new Value<>("BStringValue"));
        adapter.persist(dTwo);
        adapter.commit();

        try (RepositoryConnection connection = repo.getConnection()) {
            final Resource subj = vf.createIRI(SUBJECT.getIdentifier().toString());
            final List<Statement> classAssertions = connection.getStatements(subj, RDF.TYPE, null, false)
                                                              .stream().toList();
            final Set<URI> types = classAssertions.stream().map(s -> URI.create(s.getObject().stringValue())).collect(
                    Collectors.toSet());
            assertTrue(types.contains(tOne));
            assertTrue(types.contains(tTwo));
            try (final RepositoryResult<Statement> aStringProp = connection
                    .getStatements(subj, vf.createIRI(pOne), null, false)) {
                assertTrue(aStringProp.hasNext());
            }
            try (final RepositoryResult<Statement> bStringProp = connection
                    .getStatements(subj, vf.createIRI(pTwo), null, false)) {
                assertTrue(bStringProp.hasNext());
            }
        }
    }
}
