/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import java.util.Set;

@NamedNativeQueries({
        @NamedNativeQuery(name = "OWLClassA.findAll", query = "SELECT ?x WHERE {?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }"),
        @NamedNativeQuery(name = "OWLClassA.findByString", query = "SELECT ?x WHERE { ?x <" +
                Vocabulary.P_A_STRING_ATTRIBUTE + "> ?str . }")
})
@OWLClass(iri = Vocabulary.C_OWL_CLASS_A)
public class OWLClassA {

    @Types(fetchType = FetchType.EAGER)
    private Set<String> types;

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_A_STRING_ATTRIBUTE)
    private String stringAttribute;

    public OWLClassA() {
    }

    public OWLClassA(URI uri) {
        this.uri = uri;
    }

    /**
     * @param uri the uri to set
     */
    public void setUri(URI uri) {
        this.uri = uri;
    }

    /**
     * @return the uri
     */
    public URI getUri() {
        return uri;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setTypes(Set<String> types) {
        this.types = types;
    }

    public Set<String> getTypes() {
        return types;
    }

    @Override
    public String toString() {
        String out = "OWLClassA: uri = " + uri;
        out += ", stringAttribute = " + stringAttribute;
        if (types != null) {
            out += ", types = {" + types.toString() + "}";
        }
        return out;
    }

    public static String getClassIri() {
        return OWLClassA.class.getDeclaredAnnotation(OWLClass.class).iri();
    }
}
