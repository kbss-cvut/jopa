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

@SparqlResultSetMapping(name = OWLClassWithQueryAttr2.MAPPING_NAME, entities = {
        @EntityResult(entityClass = OWLClassWithQueryAttr2.class, fields = {
                @FieldResult(name = "uri", variable = "x"),
                @FieldResult(name = "entityAttribute", variable = "y"),
                @FieldResult(name = "entityQueryAttribute", variable = "z")
        })
})
@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr2)
public class OWLClassWithQueryAttr2 implements HasUri {

    public static final String MAPPING_NAME = "OWLClassWithQueryAttr2.entityMapping";

    private static final String QUERY_ENTITY = "SELECT ?x\n" +
            "WHERE {?this <" + Vocabulary.P_HAS_OWL_CLASS_A + "> ?x }";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, fetch = FetchType.EAGER)
    private OWLClassA entityAttribute;

    @Sparql(query = QUERY_ENTITY)
    private OWLClassA entityQueryAttribute;

    public OWLClassWithQueryAttr2() {
    }

    public OWLClassWithQueryAttr2(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getUri() {
        return uri;
    }

    public OWLClassA getEntityAttribute() {
        return entityAttribute;
    }

    public void setEntityAttribute(OWLClassA entityAttribute) {
        this.entityAttribute = entityAttribute;
    }

    public OWLClassA getEntityQueryAttribute() {
        return entityQueryAttribute;
    }

    public void setEntityQueryAttribute(OWLClassA entityQueryAttribute) {
        this.entityQueryAttribute = entityQueryAttribute;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr2: uri = " + uri;
        out += ", entityAttribute = " + entityAttribute;
        return out;
    }
}
