package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URI;

class AxiomLoader {

    private final StorageConnector connector;

    AxiomLoader(StorageConnector connector) {
        this.connector = connector;
    }

    boolean contains(Axiom<?> axiom, URI context) {
        // TODO This does not take into account inferred statements
        final Resource subject = ResourceFactory.createResource(axiom.getSubject().getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(axiom.getAssertion().getIdentifier().toString());
        final RDFNode object = JenaUtils.valueToRdfNode(axiom.getAssertion(), axiom.getValue());
        if (context != null) {
            return connector.contains(subject, property, object, context.toString());
        } else {
            return connector.contains(subject, property, object);
        }
    }
}
