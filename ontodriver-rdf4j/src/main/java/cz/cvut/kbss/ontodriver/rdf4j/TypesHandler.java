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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class TypesHandler {

    private final Connector connector;
    private final ValueFactory valueFactory;

    TypesHandler(Connector connector, ValueFactory valueFactory) {
        this.connector = connector;
        this.valueFactory = valueFactory;
    }

    Set<Axiom<URI>> getTypes(NamedResource individual, Collection<URI> contexts, boolean includeInferred)
            throws Rdf4jDriverException {
        final Collection<Statement> statements = getTypesStatements(individual, contexts, includeInferred);
        if (statements.isEmpty()) {
            return Collections.emptySet();
        }
        return resolveTypes(individual, includeInferred, statements);
    }

    private Collection<Statement> getTypesStatements(NamedResource individual, Collection<URI> contexts,
                                                     boolean includeInferred)
            throws Rdf4jDriverException {
        final Resource subject = Rdf4jUtils.toRdf4jIri(individual.getIdentifier(), valueFactory);
        return connector.findStatements(subject, RDF.TYPE, null, includeInferred,
                contexts.stream().map(c -> Rdf4jUtils.toRdf4jIri(c, valueFactory)).collect(Collectors.toSet()));
    }

    private static Set<Axiom<URI>> resolveTypes(NamedResource individual, boolean includeInferred,
                                                Collection<Statement> statements) {
        final Set<Axiom<URI>> types = new HashSet<>(statements.size());
        final Assertion clsAssertion = Assertion.createClassAssertion(includeInferred);
        for (Statement stmt : statements) {
            assert stmt.getObject() instanceof Resource;
            final URI type = Rdf4jUtils.toJavaUri((Resource) stmt.getObject());
            if (type == null) {
                // It was a blank node
                continue;
            }
            types.add(new AxiomImpl<>(individual, clsAssertion, new Value<>(type)));
        }
        return types;
    }

    void addTypes(NamedResource individual, URI context, Set<URI> types) throws Rdf4jDriverException {
        final Collection<Statement> statements = prepareSesameStatements(individual, context, types);
        connector.addStatements(statements);
    }

    private Collection<Statement> prepareSesameStatements(NamedResource individual, URI context, Set<URI> types) {
        final org.eclipse.rdf4j.model.IRI subject = Rdf4jUtils.toRdf4jIri(individual.getIdentifier(), valueFactory);
        final org.eclipse.rdf4j.model.IRI contextUri = Rdf4jUtils.toRdf4jIri(context, valueFactory);
        final Collection<Statement> statements = new ArrayList<>(types.size());
        for (URI type : types) {
            statements.add(valueFactory
                    .createStatement(subject, RDF.TYPE, valueFactory.createIRI(type.toString()), contextUri));
        }
        return statements;
    }

    void removeTypes(NamedResource individual, URI context, Set<URI> types) throws Rdf4jDriverException {
        final Collection<Statement> statements = prepareSesameStatements(individual, context, types);
        connector.removeStatements(statements);
    }
}
