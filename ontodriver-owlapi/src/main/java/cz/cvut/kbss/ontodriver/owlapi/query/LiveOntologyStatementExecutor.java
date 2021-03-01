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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import org.semanticweb.owlapi.model.OWLObject;

public class LiveOntologyStatementExecutor implements StatementExecutor {

    private final Connector connector;

    public LiveOntologyStatementExecutor(Connector connector) {
        this.connector = connector;
    }

    @Override
    public ResultSet executeQuery(final String query, final Statement statement) throws OwlapiDriverException {
        final ResultSet resultSet = connector.executeRead(snapshot -> {
            if (snapshot.getReasoner() == null) {
                throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
            }
            final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(snapshot.getOntologyManager(),
                    snapshot.getOntology(), snapshot.getReasoner());

            final QueryResult<OWLObject> res = OWL2QueryEngine.exec(query, ont);

            return res != null ? AbstractResultSet.createResultSet(res, statement, query) : null;
        });
        if (resultSet == null) {
            throw new OwlapiDriverException("Unable to execute query " + query);
        }
        return resultSet;
    }

    @Override
    public void executeUpdate(final String update) {
        connector.executeWrite(snapshot -> {
            if (snapshot.getReasoner() == null) {
                throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
            }
            final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(snapshot.getOntologyManager(),
                    snapshot.getOntology(), snapshot.getReasoner());

            OWL2QueryEngine.exec(update, ont);
        });
    }
}
