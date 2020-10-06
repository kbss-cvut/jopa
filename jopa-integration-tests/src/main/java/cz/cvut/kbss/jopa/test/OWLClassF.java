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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_F)
public class OWLClassF {

    @Id
    private URI uri;

    @Inferred
    @OWLDataProperty(iri = "http://F-secondStringAttribute")
    private String secondStringAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_F_HAS_SIMPLE_SET)
    private Set<OWLClassA> simpleSet;

    public OWLClassF() {
    }

    public OWLClassF(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getSecondStringAttribute() {
        return secondStringAttribute;
    }

    public void setSecondStringAttribute(String secondStringAttribute) {
        this.secondStringAttribute = secondStringAttribute;
    }

    public Set<OWLClassA> getSimpleSet() {
        if (simpleSet == null) {
            this.simpleSet = new HashSet<>();
        }
        return simpleSet;
    }

    public void setSimpleSet(Set<OWLClassA> simpleSet) {
        this.simpleSet = simpleSet;
    }

    @Override
    public String toString() {
        String out = "OWLClassF: uri = " + uri;
        out += ", secondStringAttribute = " + secondStringAttribute;
        return out;
    }

    public static Field getSimpleSetField() throws NoSuchFieldException {
        return OWLClassF.class.getDeclaredField("simpleSet");
    }
}
