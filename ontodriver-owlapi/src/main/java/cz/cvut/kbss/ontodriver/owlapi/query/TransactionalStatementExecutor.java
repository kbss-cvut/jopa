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
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.QueryResult;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

public class TransactionalStatementExecutor implements StatementExecutor {

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLReasoner reasoner;

    public TransactionalStatementExecutor(OntologySnapshot snapshot) {
        this.ontology = snapshot.getOntology();
        this.ontologyManager = snapshot.getOntologyManager();
        this.reasoner = snapshot.getReasoner();
    }

    @Override
    public ResultSet executeQuery(QuerySpecification query) throws OwlapiDriverException {
        final QueryResult<OWLObject> res = execute(query);
        return AbstractResultSet.createResultSet(res, query.getStatement(), query.getQuery());
    }

    private QueryResult<OWLObject> execute(QuerySpecification query) throws OwlapiDriverException {
        if (reasoner == null) {
            throw new ReasonerNotAvailableException("Cannot execute query without a reasoner.");
        }
        final OWLReasoner reasonerToUse = query.isDisableInference() ? getNoInferenceReasoner() : reasoner;
        final OWLAPIv3OWL2Ontology ont = new OWLAPIv3OWL2Ontology(ontologyManager, ontology, reasonerToUse);

        final QueryResult<OWLObject> res = OWL2QueryEngine.exec(query.getQuery(), ont);
        if (res == null) {
            throw new OwlapiDriverException("Unable to evaluate statement " + query);
        }
        return res;
    }

    private OWLReasoner getNoInferenceReasoner() {
        return new NoOpReasoner(ontology);
    }

    @Override
    public void executeUpdate(QuerySpecification query) throws OwlapiDriverException {
        execute(query);
    }
}
