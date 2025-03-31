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
package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.RemoveAxiom;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Objects;

/**
 * Remove axiom wrapper which allows us to set ontology to which it is applied.
 */
public class MutableRemoveAxiom extends RemoveAxiom implements MutableAxiomChange {

    private OWLOntology ontology;

    /**
     * @param ont   the ontology to which the change is to be applied
     * @param axiom Axiom to remove
     */
    public MutableRemoveAxiom(OWLOntology ont, OWLAxiom axiom) {
        super(ont, axiom);
        this.ontology = ont;
    }

    @Override
    public void setOntology(OWLOntology ontology) {
        this.ontology = ontology;
    }

    @Override
    public List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology) {
        this.ontology = targetOntology;
        return List.of(this);
    }

    @Nonnull
    @Override
    public OWLOntology getOntology() {
        return ontology;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MutableRemoveAxiom that)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        return Objects.equals(ontology, that.ontology) && Objects.equals(getAxiom(), that.getAxiom());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), ontology);
    }
}
