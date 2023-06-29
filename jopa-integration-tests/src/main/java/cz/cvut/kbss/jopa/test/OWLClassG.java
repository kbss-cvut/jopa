/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_G)
public class OWLClassG implements HasUri {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_H, fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    // @ParticipationConstraints({
    // @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
    // max=1)
    // })
    private OWLClassH owlClassH;

    public OWLClassG() {
    }

    public OWLClassG(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public void setOwlClassH(OWLClassH owlClassH) {
        this.owlClassH = owlClassH;
    }

    public OWLClassH getOwlClassH() {
        return owlClassH;
    }

    @Override
    public String toString() {
        return "OWLClassG{" +
                "uri=" + uri +
                ", owlClassH=" + owlClassH +
                '}';
    }
}
