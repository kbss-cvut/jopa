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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.Statement;

public class StatementExecutorFactory {

    private final OntologySnapshot transactionalSnapshot;
    private final Connector connector;

    public StatementExecutorFactory(OntologySnapshot transactionalSnapshot, Connector connector) {
        this.transactionalSnapshot = transactionalSnapshot;
        this.connector = connector;
    }

    public StatementExecutor getStatementExecutor(Statement.StatementOntology ontology) {
        assert ontology != null;

        switch (ontology) {
            case TRANSACTIONAL:
                return new TransactionalStatementExecutor(transactionalSnapshot);
            case SHARED:
                return new LiveOntologyStatementExecutor(connector);
            default:
                throw new IllegalArgumentException("Unsupported statement ontology type " + ontology);
        }
    }
}
