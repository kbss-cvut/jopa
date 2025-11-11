package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * Dummy local model that does not do anything.
 */
class DummyLocalModel implements LocalModel {

    @Override
    public Collection<Statement> enhanceStatements(Collection<Statement> statements, Resource subject,
                                                   Property property, RDFNode value, Collection<String> contexts) {
        return List.of();
    }

    @Override
    public Containment contains(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        return Containment.UNKNOWN;
    }

    @Override
    public void addStatements(List<Statement> statements, String context) {
        // Do nothing
    }

    @Override
    public void removeStatements(List<Statement> statements, String context) {
        // Do nothing
    }

    @Override
    public void removePropertyValues(Collection<SubjectPredicateContext> toRemove) {
        // Do nothing
    }

    @Override
    public Dataset getAdded() {
        return DatasetFactory.empty();
    }

    @Override
    public Dataset getRemoved() {
        return DatasetFactory.empty();
    }

    @Override
    public Set<SubjectPredicateContext> getRemovedSubjectPredicateStatements() {
        return Set.of();
    }

    @Override
    public List<String> getContexts() {
        return List.of();
    }
}
