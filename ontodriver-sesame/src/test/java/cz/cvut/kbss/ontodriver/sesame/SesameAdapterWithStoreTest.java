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

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactoryImpl;
import org.eclipse.rdf4j.common.iteration.Iterations;
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

public class SesameAdapterWithStoreTest {

    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");

    private Repository repo;
    private ValueFactory vf;

    private ConnectorFactory factory;
    private Connector connector;
    private SesameAdapter adapter;

    @BeforeEach
    public void setUp() throws Exception {
        final OntologyStorageProperties sp = OntologyStorageProperties.driver(SesameDataSource.class.getName())
                .physicalUri("memory-store").build();
        final DriverConfiguration configuration = new DriverConfiguration(sp);
        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.toString(true));
        this.factory = new ConnectorFactoryImpl(configuration);
        this.connector = factory.createStorageConnector();
        this.adapter = new SesameAdapter(connector, new RuntimeConfiguration(configuration));
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
            final List<Statement> classAssertions = Iterations.asList(connection
                    .getStatements(subj, RDF.TYPE, null, false));
            final Set<URI> types = classAssertions.stream().map(s -> URI.create(s.getObject().stringValue())).collect(
                    Collectors.toSet());
            assertTrue(types.contains(tOne));
            assertTrue(types.contains(tTwo));
            final RepositoryResult<Statement> aStringProp = connection
                    .getStatements(subj, vf.createIRI(pOne), null, false);
            assertTrue(aStringProp.hasNext());
            final RepositoryResult<Statement> bStringProp = connection
                    .getStatements(subj, vf.createIRI(pTwo), null, false);
            assertTrue(bStringProp.hasNext());
        }
    }
}
