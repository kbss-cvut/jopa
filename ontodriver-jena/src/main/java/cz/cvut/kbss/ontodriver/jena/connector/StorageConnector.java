package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

public interface StorageConnector extends Closeable {

    /**
     * Begins a transaction.
     */
    void begin();

    /**
     * Commits the current transaction.
     */
    void commit();

    /**
     * Rolls back the current transaction.
     */
    void rollback();

    /**
     * Retrieves statements corresponding to the specified criteria.
     * <p>
     * All the parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @return Collection of matching statements
     */
    Collection<Statement> find(Resource subject, Property property, RDFNode value);

    /**
     * Retrieves statements corresponding to the specified criteria from the specified named graph.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph URI
     * @return Collection of matching statements
     */
    Collection<Statement> find(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether the storage contains any statements matching the specified criteria.
     * <p>
     * All the parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean contains(Resource subject, Property property, RDFNode value);

    /**
     * Checks whether the specified context (named graph) contains any statements matching the specified criteria.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph URI
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean contains(Resource subject, Property property, RDFNode value, String context);

    /**
     * Adds the specified statements to the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to add
     */
    void add(Collection<Statement> statements);

    /**
     * Removes the specified statements from the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to remove
     */
    void remove(Collection<Statement> statements);
}
