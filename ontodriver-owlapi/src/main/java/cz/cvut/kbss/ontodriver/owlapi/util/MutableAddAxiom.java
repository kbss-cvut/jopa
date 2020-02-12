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
package cz.cvut.kbss.ontodriver.owlapi.util;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import javax.annotation.Nonnull;
import java.util.Objects;

/**
 * Add axiom wrapper which allows us to set ontology to which it is applied.
 */
public class MutableAddAxiom extends AddAxiom implements MutableAxiomChange {

    private OWLOntology ontology;

    /**
     * @param ont   the ontology to which the change is to be applied
     * @param axiom The added axiom
     */
    public MutableAddAxiom(OWLOntology ont, OWLAxiom axiom) {
        super(ont, axiom);
        this.ontology = ont;
    }

    @Override
    public void setOntology(OWLOntology ontology) {
        this.ontology = ontology;
    }

    @Nonnull
    @Override
    public OWLOntology getOntology() {
        return ontology;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof MutableAddAxiom)) return false;
        if (!super.equals(o)) return false;
        MutableAddAxiom that = (MutableAddAxiom) o;
        return Objects.equals(ontology, that.ontology) && Objects.equals(getAxiom(), that.getAxiom());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), ontology);
    }
}
