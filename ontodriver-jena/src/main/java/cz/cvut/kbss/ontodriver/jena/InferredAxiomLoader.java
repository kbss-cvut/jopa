/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class InferredAxiomLoader extends AbstractAxiomLoader {

    private final InferredStorageConnector connector;

    InferredAxiomLoader(InferredStorageConnector connector, String language) {
        super(language);
        this.connector = connector;
        this.inferred = true;
    }

    @Override
    boolean contains(Resource subject, Property property, RDFNode object, URI context) {
        return connector.containsWithInference(subject, property, object, context != null ? context.toString() : null);
    }

    @Override
    List<Axiom<?>> find(AxiomDescriptor descriptor, Map<String, Assertion> assertions) {
        final List<Axiom<?>> result = new ArrayList<>();
        final Resource subject = createResource(descriptor.getSubject().getIdentifier().toString());
        for (Assertion a : assertions.values()) {
            final Property property = createProperty(a.getIdentifier().toString());
            final Collection<Statement> statements =
                    findStatements(subject, property, descriptor.getAssertionContext(a));
            statements.forEach(s -> {
                final Optional<Value<?>> value = resolveValue(a, s.getObject());
                value.ifPresent(v -> result.add(new AxiomImpl<>(descriptor.getSubject(), a, v)));
            });
        }
        return result;
    }

    @Override
    Collection<Statement> findStatements(Resource subject, Property property, URI context) {
        return connector.findWithInference(subject, property, null, context != null ? context.toString() : null);
    }
}
