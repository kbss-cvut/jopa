package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Objects;
import java.util.Set;

public final class SubjectPredicateContext {

    private final Resource subject;

    private final Property predicate;

    private final Set<String> contexts;

    public SubjectPredicateContext(Resource subject, Property predicate, Set<String> contexts) {
        assert subject != null;
        assert contexts != null;

        this.subject = subject;
        this.predicate = predicate;
        this.contexts = contexts;
    }

    public Resource getSubject() {
        return subject;
    }

    public Property getPredicate() {
        return predicate;
    }

    public Set<String> getContexts() {
        return contexts;
    }

    public boolean matches(Statement s, String context) {
        return subject.equals(s.getSubject()) && predicate.equals(s.getPredicate()) &&
                (contexts.isEmpty() || contexts.contains(context));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof SubjectPredicateContext)) return false;
        SubjectPredicateContext that = (SubjectPredicateContext) o;
        return subject.equals(that.subject) && Objects.equals(predicate, that.predicate) && contexts.equals(
                that.contexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, predicate, contexts);
    }
}
