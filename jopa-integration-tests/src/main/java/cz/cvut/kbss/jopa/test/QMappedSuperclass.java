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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.net.URI;

@MappedSuperclass
class QMappedSuperclass implements HasUri {

    @Id(generated = true)
    private URI uri;

    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String label;

    @OWLDataProperty(iri = Vocabulary.P_Q_PARENT_STRING_ATTRIBUTE)
    private String parentString;

    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, fetch = FetchType.EAGER)
    private OWLClassA owlClassA;

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getParentString() {
        return parentString;
    }

    public void setParentString(String parentString) {
        this.parentString = parentString;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    @Override
    public String toString() {
        return "QMappedSuperclass{" +
                "uri=" + uri +
                ", label='" + label + '\'' +
                ", parentString='" + parentString + '\'' +
                ", owlClassA=" + owlClassA +
                '}';
    }
}
