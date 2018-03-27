package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;

class ReferencedListIterator extends AbstractListIterator {

    private final Property hasContent;
    private final Assertion hasContentAssertion;

    ReferencedListIterator(ReferencedListDescriptor descriptor, StorageConnector connector) {
        super(descriptor, connector);
        this.hasContentAssertion = descriptor.getNodeContent();
        this.hasContent = createProperty(hasContentAssertion.getIdentifier().toString());
    }

    @Override
    Axiom<NamedResource> nextAxiom() {
        final NamedResource value = nextValue();
        final NamedResource node = NamedResource.create(currentNode.getURI());
        return new AxiomImpl<>(node, hasContentAssertion, new Value<>(value));
    }

    @Override
    NamedResource nextValue() {
        resolveNextListNode();
        return NamedResource.create(resolveNodeContent().getURI());
    }

    private Resource resolveNodeContent() {
        final Collection<Statement> contentStatements;
        if (context != null) {
            contentStatements = connector.find(currentNode, hasContent, null, context);
        } else {
            contentStatements = connector.find(currentNode, hasContent, null);
        }
        verifyContentValueCount(contentStatements);
        final Statement statement = contentStatements.iterator().next();
        assert statement.getObject().isResource();
        return statement.getObject().asResource();
    }

    private void verifyContentValueCount(Collection<Statement> contentStatements) {
        if (contentStatements.isEmpty()) {
            throw new IntegrityConstraintViolatedException("No content found for list node " + currentNode.getURI());
        }
        if (contentStatements.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Encountered multiple content values of list node " + currentNode.getURI());
        }
    }

    @Override
    void replace(Resource replacement) {

    }
}
