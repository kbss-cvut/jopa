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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.exception.OntologyChangeApplicationException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.parameters.ChangeApplied;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.List;
import java.util.stream.Collectors;

public class OntologySnapshot {

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;
    private final OWLDataFactory dataFactory;
    private final OWLReasoner reasoner;

    public OntologySnapshot(OWLOntology ontology, OWLOntologyManager ontologyManager, OWLDataFactory dataFactory,
                            OWLReasoner reasoner) {
        this.ontology = ontology;
        this.ontologyManager = ontologyManager;
        this.dataFactory = dataFactory;
        this.reasoner = reasoner;
    }

    public OWLOntology getOntology() {
        return ontology;
    }

    public OWLOntologyManager getOntologyManager() {
        return ontologyManager;
    }

    public OWLDataFactory getDataFactory() {
        return dataFactory;
    }

    public OWLReasoner getReasoner() {
        return reasoner;
    }

    /**
     * Applies the specified changes to this ontology snapshot.
     *
     * @param changes The changes to apply
     * @return The applied changes
     */
    public List<TransactionalChange> applyChanges(List<TransactionalChange> changes) {
        final List<OWLOntologyChange> toApply = changes.stream()
                                                       .flatMap(o -> o.toOwlChanges(ontology).stream())
                                                       .collect(Collectors.toList());
        final ChangeApplied result = ontologyManager.applyChanges(toApply);
        if (result == ChangeApplied.UNSUCCESSFULLY) {
            throw new OntologyChangeApplicationException(
                    "At least one of the following changes could not have been applied to this ontology snapshot: " +
                            toApply);
        }
        return changes;
    }
}
