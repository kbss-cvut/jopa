package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class TypesHandler implements Types {
    // TODO We are not dealing with inferred statements, yet

    private final StorageConnector connector;

    TypesHandler(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) {
        final Collection<Statement> statements = getStatements(individual, context);
        final Assertion assertion = Assertion.createClassAssertion(includeInferred);
        // Skip possible non-resources and anonymous resources (not likely to appear, but safety first)
        return statements.stream().filter(s -> s.getObject().isResource() && !s.getObject().isAnon())
                         .map(s -> new AxiomImpl<>(individual, assertion,
                                 new Value<>(URI.create(s.getObject().asResource().getURI())))).collect(
                        Collectors.toSet());
    }

    private Collection<Statement> getStatements(NamedResource individual, URI context) {
        final Resource subject = ResourceFactory.createResource(individual.getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(Vocabulary.RDF_TYPE);
        return connector.find(subject, property, null, context != null ? context.toString() : null);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) {
        final List<Statement> statements = generateStatementsForTypes(individual, types);
        connector.add(statements, context != null ? context.toString() : null);
    }

    private List<Statement> generateStatementsForTypes(NamedResource individual, Set<URI> types) {
        final Resource subject = ResourceFactory.createResource(individual.getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(Vocabulary.RDF_TYPE);
        return types.stream().map(t -> ResourceFactory
                .createStatement(subject, property, ResourceFactory.createResource(t.toString()))).collect(
                Collectors.toList());
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) {
        final List<Statement> statements = generateStatementsForTypes(individual, types);
        connector.remove(statements, context != null ? context.toString() : null);
    }
}
