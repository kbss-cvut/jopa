package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

public interface InferredStorageConnector extends StatementExecutor {

    /**
     * Retrieves statements corresponding to the specified criteria from the specified named graph.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code context} is also optional, its absence means that the default graph should be used.
     *
     * @param subject  Statement subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph URI, optional
     * @return Collection of matching statements, including inferred ones
     */
    Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether the specified context (named graph) contains any statements matching the specified criteria, either asserted or inferred.
     * <p>
     * The first three parameters are optional, their absence signifies that any value in that position is acceptable.
     * <p>
     * {@code context} is also optional, its absence means that the default graph should be used.
     *
     * @param subject  Subject, optional
     * @param property Property, optional
     * @param value    Value, optional
     * @param context  Named graph IRI, optional
     * @return {@code true} if at least one statement matches the criteria, {@code false} otherwise
     */
    boolean containsWithInference(Resource subject, Property property, RDFNode value, String context);

    /**
     * Checks whether named graph with the specified IRI is consistent.
     *
     * @param context Named graph IRI, optional
     * @return Whether the graph is consistent
     */
    boolean isConsistent(String context);
}
