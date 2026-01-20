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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.repository.RepositoryException;

import java.util.Set;

public class GraphDBStorageConnection extends StorageConnection {

    public GraphDBStorageConnection(StorageConnector storageConnector,
                                    IsolationLevel isolationLevel) throws Rdf4jDriverException {
        super(storageConnector, isolationLevel);
        validateIsolationLevel(isolationLevel);
    }

    private static void validateIsolationLevel(IsolationLevel isolationLevel) throws Rdf4jDriverException {
        // GraphDB supports only READ_COMMITTED isolation level
        if (isolationLevel != null && isolationLevel.isCompatibleWith(IsolationLevels.SNAPSHOT_READ)) {
            throw new Rdf4jDriverException("GraphDBStorageConnection does not support isolation level " + isolationLevel);
        }
    }

    @Override
    boolean shouldBeginRepositoryLevelTransaction() {
        return !isReadOnly();
    }

    @Override
    public boolean isInferred(Statement statement, Set<IRI> contexts) throws Rdf4jDriverException {
        return withConnection(conn -> {
            try {
                final IRI[] ctxArr = contexts.toArray(new IRI[0]);
                // Inferred statements are in the implicit graph in GraphDB. This graph is not accessible via the RDF4J API
                return conn.hasStatement(statement, true) && !conn.hasStatement(statement, false, ctxArr);
            } catch (RepositoryException e) {
                throw new Rdf4jDriverException(e);
            }
        });
    }

    @Override
    public String getProductName() {
        return "GraphDB";
    }
}
