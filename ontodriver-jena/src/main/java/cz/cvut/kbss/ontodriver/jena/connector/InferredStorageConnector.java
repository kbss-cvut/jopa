package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

public interface InferredStorageConnector {

    /**
     * Retrieves statements corresponding to the specified criteria.
     * <p>
     * All the parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @return Collection of matching statements, including inferred ones
     */
    Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value);

    /**
     * Retrieves statements corresponding to the specified criteria from the specified named graph.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph URI
     * @return Collection of matching statements, including inferred ones
     */
    Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether the storage contains any statements matching the specified criteria, either asserted or inferred.
     * <p>
     * All the parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean containsWithInference(Resource subject, Property property, RDFNode value);

    /**
     * Checks whether the specified context (named graph) contains any statements matching the specified criteria, either asserted or inferred.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph URI
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean containsWithInference(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether the default graph is logically consistent.
     * @return Whether the graph is consistent
     */
    boolean isConsistent();

    /**
     * Checks whether named graph with the specified IRI is consistent.
     * @param context Named graph IRI
     * @return Whether the graph is consistent
     */
    boolean isConsistent(String context);
}
