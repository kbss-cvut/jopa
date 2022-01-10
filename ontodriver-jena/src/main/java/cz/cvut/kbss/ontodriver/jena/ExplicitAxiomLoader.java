/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class ExplicitAxiomLoader extends AbstractAxiomLoader {

    private static final Assertion UNSPECIFIED_ASSERTION = Assertion.createUnspecifiedPropertyAssertion(false);

    private final StorageConnector connector;

    private Map<String, Assertion> assertedProperties;
    private Assertion unspecifiedProperty;

    ExplicitAxiomLoader(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    boolean contains(Resource subject, Property property, RDFNode object, Set<URI> contexts) {
        return connector
                .contains(subject, property, object, contexts.stream().map(URI::toString).collect(Collectors.toSet()));
    }

    @Override
    Collection<Axiom<?>> find(AxiomDescriptor descriptor, Map<String, Assertion> assertions) {
        this.assertedProperties = assertions;
        this.unspecifiedProperty = resolveUnspecifiedProperty();
        final Resource subject = ResourceFactory.createResource(descriptor.getSubject().getIdentifier().toString());
        final Collection<Statement> statements = findStatements(subject, null, descriptor.getSubjectContexts());
        final List<Axiom<?>> result = transformStatementsToAxioms(descriptor, statements);
        result.addAll(loadAxiomsForPropertiesInContext(descriptor, subject));
        return result;
    }

    private Assertion resolveUnspecifiedProperty() {
        final Optional<Assertion> unspecified =
                assertedProperties.values().stream().filter(a -> a.equals(UNSPECIFIED_ASSERTION)).findAny();
        return unspecified.orElse(null);
    }

    @Override
    Collection<Statement> findStatements(Resource subject, Property property, Collection<URI> contexts) {
        return connector
                .find(subject, property, null, contexts.stream().map(URI::toString).collect(Collectors.toSet()));
    }

    private List<Axiom<?>> transformStatementsToAxioms(AxiomDescriptor descriptor, Collection<Statement> statements) {
        final List<Axiom<?>> axioms = new ArrayList<>(statements.size());
        for (Statement statement : statements) {
            final Property property = statement.getPredicate();
            if (shouldSkipProperty(property, descriptor)) {
                continue;
            }
            final Assertion a =
                    assertedProperties.containsKey(property.getURI()) ? assertedProperties.get(property.getURI()) :
                    createAssertionForStatement(statement);
            final Optional<Value<?>> value = resolveValue(a, statement.getObject());
            value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
        }
        return axioms;
    }

    private boolean shouldSkipProperty(Property property, AxiomDescriptor descriptor) {
        final String propertyUri = property.getURI();
        // If the property is not mapped and either there is not the unspecified property or the property is rdf:type,
        // which is handled by Types
        if (!assertedProperties.containsKey(propertyUri) && (unspecifiedProperty == null || propertyUri
                .equals(Vocabulary.RDF_TYPE))) {
            return true;
        }
        final Assertion a = assertedProperties.getOrDefault(propertyUri, unspecifiedProperty);
        return !assertionContextMatchesSubject(descriptor.getSubjectContexts(), descriptor.getAssertionContexts(a));
    }

    private List<Axiom<?>> loadAxiomsForPropertiesInContext(AxiomDescriptor descriptor, Resource subject) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        for (Assertion a : assertedProperties.values()) {
            final Set<URI> assertionCtx = descriptor.getAssertionContexts(a);
            if (assertionContextMatchesSubject(descriptor.getSubjectContexts(), assertionCtx)) {
                continue;
            }
            final Property property = ResourceFactory.createProperty(a.getIdentifier().toString());
            final Collection<Statement> statements = findStatements(subject, property, assertionCtx);
            statements.forEach(statement -> {
                final Optional<Value<?>> value = resolveValue(a, statement.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            });
        }
        if (unspecifiedProperty != null && !assertionContextMatchesSubject(descriptor.getSubjectContexts(),
                descriptor.getAssertionContexts(unspecifiedProperty))) {
            final Collection<Statement> statements =
                    findStatements(subject, null, descriptor.getAssertionContexts(unspecifiedProperty));
            for (Statement s : statements) {
                final Assertion a = createAssertionForStatement(s);
                final Optional<Value<?>> value = resolveValue(a, s.getObject());
                value.ifPresent(v -> axioms.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            }
        }
        return axioms;
    }

    private static boolean assertionContextMatchesSubject(Set<URI> subjectCtx, Set<URI> assertionCtx) {
        return (subjectCtx.isEmpty() && assertionCtx.isEmpty()) ||
                (!subjectCtx.isEmpty() && assertionCtx.stream().anyMatch(subjectCtx::contains));
    }
}
