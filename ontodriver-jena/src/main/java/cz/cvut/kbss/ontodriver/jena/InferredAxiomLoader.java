package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class InferredAxiomLoader extends AbstractAxiomLoader {

    private final InferredStorageConnector connector;

    InferredAxiomLoader(InferredStorageConnector connector) {
        this.connector = connector;
        this.inferred = true;
    }

    @Override
    boolean contains(Resource subject, Property property, RDFNode object, URI context) {
        return connector.containsWithInference(subject, property, object, context != null ? context.toString() : null);
    }

    @Override
    List<Axiom<?>> find(AxiomDescriptor descriptor, Map<String, Assertion> assertions) {
        final List<Axiom<?>> result = new ArrayList<>();
        final Resource subject = createResource(descriptor.getSubject().getIdentifier().toString());
        for (Assertion a : assertions.values()) {
            final Property property = createProperty(a.getIdentifier().toString());
            final Collection<Statement> statements =
                    findStatements(subject, property, descriptor.getAssertionContext(a));
            statements.forEach(s -> {
                final Optional<Value<?>> value = resolveValue(a, s.getObject());
                value.ifPresent(v -> result.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            });
        }
        return result;
    }

    @Override
    Collection<Statement> findStatements(Resource subject, Property property, URI context) {
        return connector.findWithInference(subject, property, null, context != null ? context.toString() : null);
    }
}
