/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

public class LiveOntologyStatementExecutor implements StatementExecutor {

    private final Connector connector;

    public LiveOntologyStatementExecutor(Connector connector) {
        this.connector = connector;
    }

    @Override
    public ResultSet executeQuery(QuerySpecification query) throws OwlapiDriverException {
        final ResultSet resultSet = connector.executeRead(snapshot -> {
            final QueryResult<OWLObject> res = execute(query, snapshot);

            return res != null ? AbstractResultSet.createResultSet(res, query.getStatement(), query.getQuery()) : null;
        });
        if (resultSet == null) {
            throw new OwlapiDriverException("Unable to execute query " + query);
        }
        return resultSet;
    }

    private QueryResult<OWLObject> execute(QuerySpecification query, OntologySnapshot snapshot) {
        if (snapshot.getReasoner() == null) {
            throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
        }
        final OWLReasoner reasonerToUse =
                query.isDisableInference() ? getNoInferenceReasoner(snapshot) : snapshot.getReasoner();
        final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(snapshot.getOntologyManager(),
                                                                  snapshot.getOntology(), reasonerToUse);

        return OWL2QueryEngine.exec(query.getQuery(), ont);
    }

    private static OWLReasoner getNoInferenceReasoner(OntologySnapshot snapshot) {
        return new NoOpReasoner(snapshot.getOntology());
    }

    @Override
    public void executeUpdate(QuerySpecification query) {
        connector.executeWrite(snapshot -> execute(query, snapshot));
    }
}
