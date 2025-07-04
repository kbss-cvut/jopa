/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import java.util.Set;

/**
 * Represents the subject, predicate and context(s) of a statement.
 * <p>
 * Used to indicate what property values to remove from the repository.
 */
public record SubjectPredicateContext(Resource subject, Property predicate, Set<String> contexts) {

    public boolean matches(Statement s, String context) {
        return subject.equals(s.getSubject()) && predicate.equals(s.getPredicate()) &&
                (contexts.isEmpty() || contexts.contains(context));
    }
}
