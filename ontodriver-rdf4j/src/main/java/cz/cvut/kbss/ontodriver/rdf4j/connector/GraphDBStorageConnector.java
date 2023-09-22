/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.connector.init.RepositoryConnectorInitializer;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.Set;

public class GraphDBStorageConnector extends StorageConnector {

    public GraphDBStorageConnector(RepositoryConnectorInitializer repoInitializer) {
        super(repoInitializer);
    }

    @Override
    public boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException {
        try (final RepositoryConnection conn = acquireConnection()) {
            final IRI[] ctxArr = contexts.toArray(new IRI[0]);
            // Inferred statements are in the implicit graph in GraphDB. This graph is not accessible via the RDF4J API
            return conn.hasStatement(statement, true) && !conn.hasStatement(statement, false, ctxArr);
        } catch (RepositoryException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
