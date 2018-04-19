package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URI;
import java.util.Map;
import java.util.Set;

/**
 * This class performs an epistemic removal of statements.
 * <p/>
 * Epistemic remove means that only information known to the application is
 * deleted. The assertions in the descriptor represent this information. Thus,
 * only statements representing these properties are removed from the ontology. Note that if the
 * descriptor contains an unspecified property assertion, all property
 * assertions related to the subject individual are removed from the property's
 * context.
 */
class EpistemicAxiomRemover {

    private final StorageConnector connector;

    EpistemicAxiomRemover(StorageConnector connector) {
        this.connector = connector;
    }

    /**
     * Removes statements corresponding to the subject and properties specified by the descriptor.
     *
     * @param descriptor Descriptor of statements to remove
     */
    void remove(AxiomDescriptor descriptor) {
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        descriptor.getAssertions().stream().filter(a -> !a.isInferred()).forEach(assertion -> {
            final URI context = descriptor.getAssertionContext(assertion);
            final Property property = ResourceFactory.createProperty(assertion.getIdentifier().toString());
            connector.remove(subject, property, null, context != null ? context.toString() : null);
        });
    }

    /**
     * Removes statements corresponding to the specified values.
     * <p>
     * This version removes precisely statements whose subject, property and object match the specified data.
     *
     * @param subject    Statement subject
     * @param properties Assertions to values
     * @param context    Context from which to remove the statements
     */
    void remove(NamedResource subject, Map<Assertion, Set<Value<?>>> properties, URI context) {
        final Resource resource = ResourceFactory.createResource(subject.getIdentifier().toString());
        if (context != null) {
            final String strCtx = context.toString();
            properties.forEach((assertion, values) -> {
                final Property property = ResourceFactory.createProperty(assertion.getIdentifier().toString());
                values.forEach(value -> connector
                        .remove(resource, property, JenaUtils.valueToRdfNode(assertion, value), strCtx));
            });
        } else {
            properties.forEach((assertion, values) -> {
                final Property property = ResourceFactory.createProperty(assertion.getIdentifier().toString());
                values.forEach(
                        value -> connector
                                .remove(resource, property, JenaUtils.valueToRdfNode(assertion, value), null));
            });
        }
    }
}
