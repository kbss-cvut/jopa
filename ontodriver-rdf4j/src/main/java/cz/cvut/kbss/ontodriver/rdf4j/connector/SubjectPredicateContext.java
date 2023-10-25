/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;

import java.util.Objects;
import java.util.Set;

/**
 * A combination of statement subject, predicate and context(s).
 *
 * It is used to indicate values of properties to remove.
 */
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
