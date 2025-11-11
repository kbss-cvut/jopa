package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;
import java.util.Set;

interface LocalModel {
    Collection<Statement> enhanceStatements(Collection<Statement> statements, Resource subject, Property property,
                                            RDFNode value, Collection<String> contexts);

    Containment contains(Resource subject, Property property, RDFNode value,
                                                  Collection<String> contexts);

    void addStatements(List<Statement> statements, String context);

    void removeStatements(List<Statement> statements, String context);

    void removePropertyValues(Collection<SubjectPredicateContext> toRemove);

    Dataset getAdded();

    Dataset getRemoved();

    Set<SubjectPredicateContext> getRemovedSubjectPredicateStatements();

    List<String> getContexts();

    enum Containment {
        ADDED, REMOVED, UNKNOWN
    }
}
