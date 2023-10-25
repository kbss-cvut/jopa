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

import java.net.URI;
import java.util.List;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_K)
public class OWLClassK implements HasUri {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasE")
    private OWLClassE owlClassE;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST)
    private List<OWLClassE> simpleList;

    @Sequence
    @OWLObjectProperty(iri = Vocabulary.P_HAS_REFERENCED_LIST)
    private List<OWLClassE> referencedList;

    @Override
    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public OWLClassE getOwlClassE() {
        return owlClassE;
    }

    public void setOwlClassE(OWLClassE owlClassE) {
        this.owlClassE = owlClassE;
    }

    public List<OWLClassE> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<OWLClassE> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassE> getReferencedList() {
        return referencedList;
    }

    public void setReferencedList(List<OWLClassE> referencedList) {
        this.referencedList = referencedList;
    }

    @Override
    public String toString() {
        return "[OWLClassK: " + uri + ", owlClassE = " + owlClassE + "]";
    }
}
