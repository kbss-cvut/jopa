/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
            case CENTRAL:
                return new LiveOntologyStatementExecutor(connector);
            default:
                throw new IllegalArgumentException("Unsupported statement ontology type " + ontology);
        }
    }
}
