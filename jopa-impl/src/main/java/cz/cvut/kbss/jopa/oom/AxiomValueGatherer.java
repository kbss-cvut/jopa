/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.*;

/**
 * Gathers axiom values in descriptor for persist or update.
 */
class AxiomValueGatherer {

    private final AxiomValueDescriptor axiomDescriptor;
    private final Collection<SimpleListValueDescriptor> simpleListDescriptors = new ArrayList<>();
    private final Collection<ReferencedListValueDescriptor> referencedListDescriptors = new ArrayList<>();
    private Set<URI> typesToAdd;
    private Set<URI> typesToRemove;
    private URI typesContext;
    private Map<Assertion, Set<Value<?>>> propertiesToAdd;
    private Map<Assertion, Set<Value<?>>> propertiesToRemove;
    private URI propertiesContext;

    AxiomValueGatherer(NamedResource subject, URI subjectContext) {
        this.axiomDescriptor = new AxiomValueDescriptor(subject);
        axiomDescriptor.setSubjectContext(subjectContext);
    }

    NamedResource getSubjectIdentifier() {
        return axiomDescriptor.getSubject();
    }

    void addValue(Assertion assertion, Value<?> value, URI context) {
        addValues(assertion, Collections.singleton(value), context);
    }

    void addValues(Assertion assertion, Collection<Value<?>> values, URI context) {
        axiomDescriptor.addAssertion(assertion);
        for (Value<?> v : values) {
            axiomDescriptor.addAssertionValue(assertion, v);
        }
        if (!Objects.equals(axiomDescriptor.getSubjectContext(), context)) {
            axiomDescriptor.setAssertionContext(assertion, context);
        }
    }

    void addSimpleListValues(SimpleListValueDescriptor listDescriptor) {
        simpleListDescriptors.add(listDescriptor);
    }

    void addReferencedListValues(ReferencedListValueDescriptor listDescriptor) {
        referencedListDescriptors.add(listDescriptor);
    }

    void addTypes(Set<URI> types, URI context) {
        if (typesToAdd == null) {
            this.typesToAdd = new HashSet<>(types.size());
        }
        appendTypes(typesToAdd, types, context);
    }

    private void appendTypes(Set<URI> target, Set<URI> types, URI context) {
        target.addAll(types);
        this.typesContext = context;
    }

    void removeTypes(Set<URI> types, URI context) {
        if (typesToRemove == null) {
            this.typesToRemove = new HashSet<>(types.size());
        }
        appendTypes(typesToRemove, types, context);
    }

    void addProperties(Map<Assertion, Set<Value<?>>> properties, URI context) {
        if (propertiesToAdd == null) {
            this.propertiesToAdd = new HashMap<>(properties.size());
        }
        appendProperties(propertiesToAdd, properties, context);
    }

    private void appendProperties(Map<Assertion, Set<Value<?>>> target, Map<Assertion, Set<Value<?>>> properties,
                                  URI context) {
        for (Map.Entry<Assertion, Set<Value<?>>> e : properties.entrySet()) {
            if (!target.containsKey(e.getKey())) {
                target.put(e.getKey(), e.getValue());
            } else {
                target.get(e.getKey()).addAll(e.getValue());
            }
        }
        this.propertiesContext = context;
    }

    void removeProperties(Map<Assertion, Set<Value<?>>> properties, URI context) {
        if (propertiesToRemove == null) {
            this.propertiesToRemove = new HashMap<>(properties.size());
        }
        appendProperties(propertiesToRemove, properties, context);
    }

    void persist(Connection connection) {
        try {
            connection.persist(axiomDescriptor);
            if (typesToAdd != null) {
                connection.types().addTypes(axiomDescriptor.getSubject(), typesContext, typesToAdd);
            }
            if (propertiesToAdd != null) {
                connection.properties().addProperties(axiomDescriptor.getSubject(), propertiesContext, propertiesToAdd);
            }
            for (SimpleListValueDescriptor d : simpleListDescriptors) {
                connection.lists().persistSimpleList(d);
            }
            for (ReferencedListValueDescriptor d : referencedListDescriptors) {
                connection.lists().persistReferencedList(d);
            }
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    void update(Connection connection) {
        try {
            connection.update(axiomDescriptor);
            if (typesToAdd != null) {
                connection.types().addTypes(axiomDescriptor.getSubject(), typesContext, typesToAdd);
            }
            if (typesToRemove != null) {
                connection.types().removeTypes(axiomDescriptor.getSubject(), typesContext, typesToRemove);
            }
            if (propertiesToAdd != null) {
                connection.properties().addProperties(axiomDescriptor.getSubject(), propertiesContext, propertiesToAdd);
            }
            if (propertiesToRemove != null) {
                connection.properties()
                          .removeProperties(axiomDescriptor.getSubject(), propertiesContext, propertiesToRemove);
            }
            for (SimpleListValueDescriptor d : simpleListDescriptors) {
                connection.lists().updateSimpleList(d);
            }
            for (ReferencedListValueDescriptor d : referencedListDescriptors) {
                connection.lists().updateReferencedList(d);
            }
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }
}
