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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class TypesHandler implements Types {

    private final StorageConnector connector;

    private final InferredStorageConnector inferenceConnector;

    TypesHandler(StorageConnector connector, InferredStorageConnector inferenceConnector) {
        this.connector = connector;
        this.inferenceConnector = inferenceConnector;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred) {
        final Collection<Statement> statements = getStatements(individual, context, includeInferred);
        final Assertion assertion = Assertion.createClassAssertion(includeInferred);
        // Skip possible non-resources and anonymous resources (not likely to appear, but safety first)
        return statements.stream().filter(s -> s.getObject().isResource() && !s.getObject().isAnon())
                         .map(s -> new AxiomImpl<>(individual, assertion,
                                 new Value<>(URI.create(s.getObject().asResource().getURI())))).collect(
                        Collectors.toSet());
    }

    private Collection<Statement> getStatements(NamedResource individual, URI context, boolean includedInferred) {
        final Resource subject = ResourceFactory.createResource(individual.getIdentifier().toString());
        final String ctx = context != null ? context.toString() : null;
        if (includedInferred) {
            return inferenceConnector.findWithInference(subject, RDF.type, null, ctx);
        } else {
            return connector.find(subject, RDF.type, null, ctx);
        }
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) {
        final List<Statement> statements = generateStatementsForTypes(individual, types);
        connector.add(statements, context != null ? context.toString() : null);
    }

    private static List<Statement> generateStatementsForTypes(NamedResource individual, Set<URI> types) {
        final Resource subject = ResourceFactory.createResource(individual.getIdentifier().toString());
        final Property property = ResourceFactory.createProperty(Vocabulary.RDF_TYPE);
        return types.stream().map(t -> ResourceFactory
                .createStatement(subject, property, ResourceFactory.createResource(t.toString()))).collect(
                Collectors.toList());
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) {
        final List<Statement> statements = generateStatementsForTypes(individual, types);
        connector.remove(statements, context != null ? context.toString() : null);
    }
}
