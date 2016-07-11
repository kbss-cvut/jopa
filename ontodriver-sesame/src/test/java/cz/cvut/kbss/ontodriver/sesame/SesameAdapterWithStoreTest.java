package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import info.aduna.iteration.Iterations;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryResult;

import java.net.URI;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.assertTrue;

public class SesameAdapterWithStoreTest {

    private static final String LANGUAGE = "en";
    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityX");

    private Repository repo;
    private ValueFactory vf;

    private ConnectorFactory factory;
    private Connector connector;
    private SesameAdapter adapter;

    @Before
    public void setUp() throws Exception {
        final OntologyStorageProperties sp = OntologyStorageProperties.driver(SesameDataSource.class.getName())
                                                                      .physicalUri("memory-store").build();
        final Configuration configuration = new Configuration(sp);
        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.toString(true));
        configuration.setProperty(ConfigParam.ONTOLOGY_LANGUAGE, LANGUAGE);
        this.factory = ConnectorFactory.getInstance();
        this.connector = factory.createStorageConnector(configuration);
        this.adapter = new SesameAdapter(connector, configuration);
        this.repo = adapter.unwrap(Repository.class);
        this.vf = repo.getValueFactory();
    }

    @After
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

        final RepositoryConnection connection = repo.getConnection();
        try {
            final Resource subj = vf.createURI(SUBJECT.getIdentifier().toString());
            final List<Statement> classAssertions = Iterations.asList(connection
                    .getStatements(subj, RDF.TYPE, null, false));
            final Set<URI> types = classAssertions.stream().map(s -> URI.create(s.getObject().stringValue())).collect(
                    Collectors.toSet());
            assertTrue(types.contains(tOne));
            assertTrue(types.contains(tTwo));
            final RepositoryResult<Statement> aStringProp = connection
                    .getStatements(subj, vf.createURI(pOne), null, false);
            assertTrue(aStringProp.hasNext());
            final RepositoryResult<Statement> bStringProp = connection
                    .getStatements(subj, vf.createURI(pTwo), null, false);
            assertTrue(bStringProp.hasNext());
        } finally {
            connection.close();
        }
    }
}
