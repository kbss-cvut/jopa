/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Objects;
import java.util.Set;

/**
 * Represents the subject, predicate and context(s) of a statement.
 * <p>
 * Used to indicate what property values to remove from the repository.
 */
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
        if (this == o) {return true;}
        if (!(o instanceof SubjectPredicateContext that)) {return false;}
        return subject.equals(that.subject) && Objects.equals(predicate, that.predicate) && contexts.equals(that.contexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, predicate, contexts);
    }
}
