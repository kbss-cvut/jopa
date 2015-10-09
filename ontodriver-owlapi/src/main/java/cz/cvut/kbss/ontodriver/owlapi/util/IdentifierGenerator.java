package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver_new.exception.IdentifierGenerationException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.util.Random;

public class IdentifierGenerator {

    private static final int GENERATION_THRESHOLD = 100;
    private static final Random RANDOM = new Random();

    private final OWLOntology ontology;

    public IdentifierGenerator(OWLOntology ontology) {
        assert ontology != null;
        this.ontology = ontology;
    }

    /**
     * Generates an identifier which is unique w.r.t. individuals in the known ontology.
     *
     * @param classUri URI of individual's class, used as base for the identifier
     * @return Unique identifier
     * @throws IdentifierGenerationException If unable to generate unique identifier
     */
    public URI generateIdentifier(URI classUri) {
        boolean unique = false;
        URI id = null;
        int counter = 0;
        while (!unique && counter++ < GENERATION_THRESHOLD) {
            if (classUri.getFragment() != null) {
                id = URI.create(classUri.toString() + "_instance" + RANDOM.nextInt());
            } else {
                String base = classUri.toString();
                if (base.endsWith("/")) {
                    id = URI.create(base + "_instance" + RANDOM.nextInt());
                } else {
                    id = URI.create(base + "#instance" + RANDOM.nextInt());
                }
            }
            unique = isIdentifierUnique(id);
        }
        if (!unique) {
            throw new IdentifierGenerationException("Unable to generate a unique identifier.");
        }
        return id;
    }

    boolean isIdentifierUnique(URI uri) {
        return !ontology.containsIndividualInSignature(IRI.create(uri));
    }
}
