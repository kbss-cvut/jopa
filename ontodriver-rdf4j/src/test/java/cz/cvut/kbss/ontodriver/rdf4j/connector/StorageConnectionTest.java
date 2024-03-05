package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.inferencer.fc.SchemaCachingRDFSInferencer;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class StorageConnectionTest {

    private Repository repository;

    private StorageConnection sut;

    @AfterEach
    void tearDown() throws Exception {
        if (repository.isInitialized()) {
            if (sut != null) {
                sut.close();
            }
            repository.shutDown();
        }
    }

    @Test
    void isInferredReturnsTrueWhenStatementIsInferredInSpecifiedContext() throws Exception {
        this.repository = new SailRepository(new SchemaCachingRDFSInferencer(new MemoryStore()));
        final StorageConnector connector = mock(StorageConnector.class);
        when(connector.acquireConnection()).thenReturn(repository.getConnection());
        this.sut = new StorageConnection(connector);

        final ValueFactory vf = SimpleValueFactory.getInstance();
        final IRI childType = vf.createIRI(Generator.generateUri().toString());
        final IRI parentType = vf.createIRI(Generator.generateUri().toString());
        final IRI instance = vf.createIRI(Generator.generateUri().toString());
        final URI context = Generator.generateUri();
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.begin();
            conn.add(childType, RDFS.SUBCLASSOF, parentType, vf.createIRI(context.toString()));
            conn.add(instance, RDF.TYPE, childType, vf.createIRI(context.toString()));
            conn.commit();
        }

        sut.begin();
        try {
            assertFalse(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.singleton(vf.createIRI(Generator.generateUri()
                                                                                                                                       .toString()))));
            assertTrue(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.singleton(vf.createIRI(context.toString()))));
            assertTrue(sut.isInferred(vf.createStatement(instance, RDF.TYPE, parentType), Collections.emptySet()));
        } finally {
            sut.rollback();
        }
    }
}
