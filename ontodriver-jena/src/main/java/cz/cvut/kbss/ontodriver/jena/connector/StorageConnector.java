package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;

public interface StorageConnector extends Closeable, Wrapper, StatementExecutor {

    /**
     * Begins a transaction.
     */
    void begin();

    /**
     * Commits the current transaction.
     */
    void commit() throws JenaDriverException;

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
     * Lists all contexts (named graph) in the repository (including the transactional ones).
     *
     * @return List of named graph URIs
     */
    List<String> getContexts();

    /**
     * Adds the specified statements to the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to add
     */
    void add(List<Statement> statements);

    /**
     * Adds the specified statements to the specified context in the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to add
     * @param context    Target context
     */
    void add(List<Statement> statements, String context);

    /**
     * Removes the specified statements from the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to remove
     */
    void remove(List<Statement> statements);

    /**
     * Removes the specified statements from the specified context in the storage.
     * <p>
     * Requires an active transaction.
     *
     * @param statements Statements to remove
     * @param context    Target context
     */
    void remove(List<Statement> statements, String context);

    /**
     * Removes statements matching the specified pattern from the storage.
     * <p>
     * This operation works with the default context.
     *
     * @param subject  Statement subject, optional
     * @param property Statement property, optional
     * @param object   Statement object, optional
     */
    void remove(Resource subject, Property property, RDFNode object);

    /**
     * Removes statements matching the specified pattern from the specified storage context.
     *
     * @param subject  Statement subject, optional
     * @param property Statement property, optional
     * @param object   Statement object, optional
     * @param context  Repository context IRI
     */
    void remove(Resource subject, Property property, RDFNode object, String context);

    @Override
    void close() throws JenaDriverException;
}
