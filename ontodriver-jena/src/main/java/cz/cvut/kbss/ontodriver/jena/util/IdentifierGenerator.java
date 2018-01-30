package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URI;

public class IdentifierGenerator {

    private static final int GENERATOR_THRESHOLD = 64;

    private final StorageConnector storageConnector;

    public IdentifierGenerator(StorageConnector storageConnector) {
        this.storageConnector = storageConnector;
    }

    /**
     * Generates a unique identifier based on the specified class URI.
     *
     * @param classUri Type URI, used as the identifier base
     * @return Generated identifier
     */
    public URI generateIdentifier(URI classUri) {
        int i = 0;
        boolean exists;
        final Property property = ResourceFactory.createProperty(Vocabulary.RDF_TYPE);
        final RDFNode type = ResourceFactory.createResource(classUri.toString());
        URI result;
        do {
            result = IdentifierUtils.generateIdentifier(classUri);
            exists = storageConnector.contains(ResourceFactory.createResource(result.toString()), property, type);
            i++;
        } while (exists && i < GENERATOR_THRESHOLD);
        if (i >= GENERATOR_THRESHOLD) {
            throw new IdentifierGenerationException(
                    "Failed to generate a unique identifier in " + GENERATOR_THRESHOLD + " attempts.");
        }
        return result;
    }
}
