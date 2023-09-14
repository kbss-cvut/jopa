package cz.cvut.kbss.ontodriver.rdf4j.connector;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;

import java.util.Objects;
import java.util.Set;

public final class SubjectPredicateContext {

    private final Resource subject;

    private final IRI predicate;

    private final Set<? extends Resource> contexts;

    public SubjectPredicateContext(Resource subject, IRI predicate, Set<? extends Resource> contexts) {
        assert subject != null;
        assert predicate != null;
        assert contexts != null;

        this.subject = subject;
        this.predicate = predicate;
        this.contexts = contexts;
    }

    public Resource getSubject() {
        return subject;
    }

    public IRI getPredicate() {
        return predicate;
    }

    public Set<? extends Resource> getContexts() {
        return contexts;
    }

    public boolean matches(Statement s) {
        return subject.equals(s.getSubject()) && predicate.equals(s.getPredicate()) &&
                (contexts.isEmpty() || contexts.contains(s.getContext()));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SubjectPredicateContext)) {
            return false;
        }
        SubjectPredicateContext that = (SubjectPredicateContext) o;
        return subject.equals(that.subject) && predicate.equals(that.predicate) && contexts.equals(that.contexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, predicate, contexts);
    }
}
