package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.util.Transaction;
import org.apache.jena.rdf.model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Transformations between OntoDriver API-based values and Jena-based ones.
 * <p>
 * Implementation notes:
 * <ul>
 * <li>Datatype literal types are based on Jena's mapping, as described in a table at <a href="https://jena.apache.org/documentation/notes/typed-literals.html">https://jena.apache.org/documentation/notes/typed-literals.html</a></li>
 * </ul>
 */
class JenaAdapter {

    private final Transaction transaction = new Transaction();

    private final StorageConnector connector;

    JenaAdapter(StorageConnector connector) {
        this.connector = connector;
    }

    void commit() {
    }

    void rollback() {
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
}
