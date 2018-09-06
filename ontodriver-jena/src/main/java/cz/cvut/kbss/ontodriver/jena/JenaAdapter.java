package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.list.ListHandler;
import cz.cvut.kbss.ontodriver.jena.query.JenaPreparedStatement;
import cz.cvut.kbss.ontodriver.jena.query.JenaStatement;
import cz.cvut.kbss.ontodriver.jena.util.IdentifierGenerator;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.util.Transaction;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Transformations between OntoDriver API-based values and Jena-based ones.
 * <p>
 * Implementation notes:
 * <ul>
 * <li>Datatype literal types are based on Jena's mapping, as described in a table at <a
 * href="https://jena.apache.org/documentation/notes/typed-literals.html">https://jena.apache.org/documentation/notes/typed-literals.html</a></li>
 * </ul>
 */
public class JenaAdapter implements Wrapper {

    private final Transaction transaction = new Transaction();

    private final StorageConnector connector;
    private final InferredStorageConnector inferenceConnector;

    private final String language;

    JenaAdapter(StorageConnector connector, InferredStorageConnector inferenceConnector, String language) {
        this.connector = connector;
        this.inferenceConnector = inferenceConnector;
        this.language = language;
    }

    void commit() throws JenaDriverException {
        if (transaction.isActive()) {
            transaction.commit();
            connector.commit();
            transaction.afterCommit();
        }
    }

    void rollback() {
        if (transaction.isActive()) {
            transaction.rollback();
            connector.rollback();
            transaction.afterRollback();
        }
    }

    void persist(AxiomValueDescriptor descriptor) {
        beginTransactionIfNotActive();
        new AxiomSaver(connector).saveAxioms(descriptor);
    }

    private void beginTransactionIfNotActive() {
        if (!transaction.isActive()) {
            connector.begin();
            transaction.begin();
        }
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        beginTransactionIfNotActive();
        return new MainAxiomLoader(connector, inferenceConnector, language).find(descriptor);
    }

    boolean contains(Axiom<?> axiom, URI context) {
        beginTransactionIfNotActive();
        return new MainAxiomLoader(connector, inferenceConnector, language).contains(axiom, context);
    }

    List<URI> getContext() {
        beginTransactionIfNotActive();
        return connector.getContexts().stream().map(URI::create).collect(Collectors.toList());
    }

    URI generateIdentifier(URI classUri) {
        beginTransactionIfNotActive();
        return new IdentifierGenerator(connector).generateIdentifier(classUri);
    }

    boolean isConsistent(URI context) {
        beginTransactionIfNotActive();
        return inferenceConnector.isConsistent(context != null ? context.toString() : null);
    }

    void update(AxiomValueDescriptor descriptor) {
        beginTransactionIfNotActive();
        new EpistemicAxiomRemover(connector).remove(descriptor);
        new AxiomSaver(connector).saveAxioms(descriptor);
    }

    void remove(AxiomDescriptor descriptor) {
        beginTransactionIfNotActive();
        new EpistemicAxiomRemover(connector).remove(descriptor);
    }

    TypesHandler typesHandler() {
        beginTransactionIfNotActive();
        return new TypesHandler(connector, inferenceConnector);
    }

    PropertiesHandler propertiesHandler() {
        beginTransactionIfNotActive();
        return new PropertiesHandler(connector, language);
    }

    public ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> simpleListHandler() {
        beginTransactionIfNotActive();
        return ListHandler.simpleListHandler(connector);
    }

    public ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> referencedListHandler() {
        beginTransactionIfNotActive();
        return ListHandler.referencedListHandler(connector);
    }

    JenaStatement createStatement() {
        beginTransactionIfNotActive();
        return new JenaStatement(inferenceConnector);
    }

    JenaPreparedStatement prepareStatement(String sparql) {
        beginTransactionIfNotActive();
        return new JenaPreparedStatement(inferenceConnector, sparql);
    }

    void close() throws JenaDriverException {
        connector.close();
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return connector.unwrap(cls);
    }
}
