/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr6)
public class OWLClassWithQueryAttr6 {

    private static final String QUERY = "SELECT ?pluralAttribute\n" +
            "WHERE {?this <" + Vocabulary.P_HAS_SIMPLE_LIST + "> ?pluralAttribute}";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST, cascade = CascadeType.ALL)
    private Set<OWLClassA> pluralAttribute;

    @Sparql(query=QUERY, fetchType = FetchType.LAZY)
    private Set<OWLClassA> pluralQueryAttribute;

    public OWLClassWithQueryAttr6() {
    }

    public OWLClassWithQueryAttr6(URI uri) {
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

    public Set<OWLClassA> getPluralAttribute() {
        return pluralAttribute;
    }

    public void setPluralAttribute(Set<OWLClassA> pluralAttribute) {
        this.pluralAttribute = pluralAttribute;
    }

    public Set<OWLClassA> getPluralQueryAttribute() {
        return pluralQueryAttribute;
    }

    public void setPluralQueryAttribute(Set<OWLClassA> pluralQueryAttribute) {
        this.pluralQueryAttribute = pluralQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr6.class.getAnnotation(OWLClass.class).iri();
    }

    public static String getSparqlQuery() {
        return QUERY;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", pluralAttribute = " + pluralAttribute;
        return out;
    }
}
