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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.c_OwlClassD)
public class OWLClassD {

    private static final String CLS_A_FIELD = "owlClassA";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_A, fetch = FetchType.EAGER)
    // @ParticipationConstraints({
    // @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
    // max=1)
    // })
    private OWLClassA owlClassA;

    public OWLClassD() {
    }

    public OWLClassD(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setOwlClassA(OWLClassA owlClassA) {
        this.owlClassA = owlClassA;
    }

    public OWLClassA getOwlClassA() {
        return owlClassA;
    }

    public static String getClassIri() {
        return OWLClassD.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws NoSuchFieldException {
        return OWLClassD.class.getDeclaredField("uri");
    }

    public static Field getOwlClassAField() throws NoSuchFieldException {
        return OWLClassD.class.getDeclaredField(CLS_A_FIELD);
    }

    @Override
    public String toString() {
        String out = "OWLClassD: uri = " + uri;
        out += ", owlClassA = " + owlClassA;
        return out;
    }
}
