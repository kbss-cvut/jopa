/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_L)
public class OWLClassL {

    @Id
    private URI uri;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence")
    @ParticipationConstraints({
                                      @ParticipationConstraint(min = 1,
                                                               owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
                              })
    private List<OWLClassA> simpleList;

    @Sequence(type = SequenceType.referenced)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasReferencedSequence")
    @ParticipationConstraints({
                                      @ParticipationConstraint(max = 2,
                                                               owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
                              })
    private List<OWLClassA> referencedList;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA")
    @ParticipationConstraints({
                                      @ParticipationConstraint(min = 1, max = 5,
                                                               owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
                              })
    private Set<OWLClassA> set;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasAExtra")
    @ParticipationConstraints({
                                      @ParticipationConstraint(min = 1,
                                                               owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
                              })
    private OWLClassA singleA;

    public OWLClassL() {
    }

    public OWLClassL(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setReferencedList(List<OWLClassA> referencedList) {
        this.referencedList = referencedList;
    }

    public Set<OWLClassA> getSet() {
        return set;
    }

    public void setSet(Set<OWLClassA> set) {
        this.set = set;
    }

    public OWLClassA getSingleA() {
        return singleA;
    }

    public void setSingleA(OWLClassA singleA) {
        this.singleA = singleA;
    }

    @Override
    public String toString() {
        return "OWLClassL{" +
                "uri=" + uri +
                ", simpleList=" + simpleList +
                ", referencedList=" + referencedList +
                ", set=" + set +
                '}';
    }
}
