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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;

import java.net.URI;
import java.util.*;

import static cz.cvut.kbss.ontodriver.model.Assertion.createDataPropertyAssertion;
import static cz.cvut.kbss.ontodriver.model.Assertion.createObjectPropertyAssertion;

abstract class AbstractAxiomLoader {

    boolean inferred = false;

    /**
     * Checks whether the storage contains the specified axiom.
     *
     * @param axiom    Axiom whose existence should be verified
     * @param contexts Contexts to search, optional
     * @return {@code true} if the axiom exists, {@code false} otherwise
     */
    boolean contains(Axiom<?> axiom, Set<URI> contexts) {
        final Resource subject = ResourceFactory.createResource(axiom.getSubject().getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(axiom.getAssertion().getIdentifier().toString());
        final RDFNode object = JenaUtils.valueToRdfNode(axiom.getAssertion(), axiom.getValue());
        return contains(subject, property, object, contexts);
    }

    abstract boolean contains(Resource subject, Property property, RDFNode object, Set<URI> context);

    Assertion createAssertionForStatement(Statement statement) {
        if (statement.getObject().isResource()) {
            return createObjectPropertyAssertion(URI.create(statement.getPredicate().getURI()), inferred);
        } else {
            return createDataPropertyAssertion(URI.create(statement.getPredicate().getURI()), inferred);
        }
    }

    /**
     * Loads statements corresponding to subject and assertions specified by the arguments.
     * <p>
     * The specified descriptor is used in context and subject resolution.
     *
     * @param descriptor Loading descriptor, contains subject and context info
     * @param assertions Assertions to load
     * @return Matching axioms
     */
    abstract Collection<Axiom<?>> find(AxiomDescriptor descriptor, Map<String, Assertion> assertions);

    /**
     * Loads all property statements with the specified subject.
     * <p>
     * Note that type assertion statements (those with property {@code rdf:type}) are skipped.
     *
     * @param subject Statement subject
     * @param context Context identifier, optional
     * @return Matching statements
     */
    Collection<Axiom<?>> find(NamedResource subject, URI context) {
        final Resource resource = ResourceFactory.createResource(subject.getIdentifier().toString());
        final Collection<Statement> statements = findStatements(resource, null,
                context != null ? Collections.singleton(context) : Collections.emptySet());
        final List<Axiom<?>> axioms = new ArrayList<>(statements.size());
        for (Statement statement : statements) {
            if (statement.getPredicate().equals(RDF.type)) {
                continue;
            }
            final Assertion a = createAssertionForStatement(statement);
            resolveValue(a, statement.getObject()).ifPresent(v -> axioms.add(new AxiomImpl<>(subject, a, v)));
        }
        return axioms;
    }

    abstract Collection<Statement> findStatements(Resource subject, Property property, Collection<URI> contexts);

    Optional<Value<?>> resolveValue(Assertion assertion, RDFNode object) {
        if (object.isResource()) {
            if (object.isAnon() || assertion.getType() == Assertion.AssertionType.DATA_PROPERTY) {
                return Optional.empty();
            }
            return Optional.of(new Value<>(NamedResource.create(object.asResource().getURI())));
        } else {
            if (shouldSkipLiteral(assertion, object)) {
                return Optional.empty();
            }
            final Object val = JenaUtils.literalToValue(object.asLiteral());
            return Optional.of(new Value<>(val));
        }
    }

    private static boolean shouldSkipLiteral(Assertion assertion, RDFNode object) {
        assert object.isLiteral();
        return assertion.getType() == Assertion.AssertionType.OBJECT_PROPERTY ||
                !doesLanguageMatch(assertion, object.asLiteral());
    }

    private static boolean doesLanguageMatch(Assertion assertion, Literal literal) {
        if (!(literal.getValue() instanceof String) || !assertion.hasLanguage()) {
            return true;
        }
        return Objects.equals(assertion.getLanguage(), literal.getLanguage()) || literal.getLanguage().isEmpty();
    }
}
