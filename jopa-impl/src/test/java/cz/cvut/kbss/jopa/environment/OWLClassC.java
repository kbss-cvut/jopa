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

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

@OWLClass(iri = Vocabulary.c_OwlClassC)
public class OWLClassC {

    private static final String REF_LIST_FIELD = "referencedList";
    private static final String SIMPLE_LIST_FIELD = "simpleList";

    @Id
    private URI uri;

    @Sequence
    @OWLObjectProperty(iri = Vocabulary.P_HAS_REFERENCED_LIST, fetch = FetchType.EAGER)
    private List<OWLClassA> referencedList;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST)
    private List<OWLClassA> simpleList;

    public OWLClassC() {
    }

    public OWLClassC(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setReferencedList(List<OWLClassA> list) {
        this.referencedList = list;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    public static String getClassIri() {
        return OWLClassC.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getRefListField() throws NoSuchFieldException,
                                                 SecurityException {
        return OWLClassC.class.getDeclaredField(REF_LIST_FIELD);
    }

    public static Field getSimpleListField() throws NoSuchFieldException,
                                                    SecurityException {
        return OWLClassC.class.getDeclaredField(SIMPLE_LIST_FIELD);
    }

    @Override
    public String toString() {
        String out = "OWLClassC: uri = " + uri;
        if (referencedList != null) {
            out += ", referencedList = {" + referencedList.toString() + "}";
        }
        if (simpleList != null) {
            out += ", simpleList = {" + simpleList.toString() + "}";
        }
        return out;
    }
}
