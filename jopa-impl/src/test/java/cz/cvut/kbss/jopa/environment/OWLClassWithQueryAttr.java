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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.c_OwlClassWithQueryAttr)
public class OWLClassWithQueryAttr {

    private static final String STR_ATT_FIELD = "stringAttribute";
    private static final String STR_QUERY_ATT_FIELD = "stringQueryAttribute";
    private static final String ENTITY_ATT_FIELD = "entityAttribute";
    private static final String ENTITY_QUERY_ATT_FIELD = "entityQueryAttribute";

    private static final String QUERY = "PREFIX jopa:<http://krizik.felk.cvut.cz/ontologies/jopa/attributes#>\n" +
            "SELECT ?stringAttribute\n" +
            "WHERE {?this jopa:B-stringAttribute ?stringAttribute}";
    private static final String QUERY_ENTITY = "SELECT ?entityAttribute\n" +
            "WHERE {?this <" + Vocabulary.P_HAS_A + "> ?entityAttribute}";

    @Id
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute")
    private String stringAttribute;

    @Sparql(query = QUERY)
    private String stringQueryAttribute;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_A, fetch = FetchType.EAGER)
    private OWLClassA entityAttribute;

    @Sparql(query = QUERY_ENTITY)
    private OWLClassA entityQueryAttribute;

    public OWLClassWithQueryAttr() {
    }

    public OWLClassWithQueryAttr(URI uri) {
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

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public String getStringQueryAttribute() {
        return stringQueryAttribute;
    }

    public void setStringQueryAttribute(String stringQueryAttribute) {
        this.stringQueryAttribute = stringQueryAttribute;
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

    public static String getClassIri() {
        return OWLClassWithQueryAttr.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
        return OWLClassWithQueryAttr.class.getDeclaredField(STR_ATT_FIELD);
    }

    public static Field getStrQueryAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr.class.getDeclaredField(STR_QUERY_ATT_FIELD);
    }

    public static Field getEntityAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr.class.getDeclaredField(ENTITY_ATT_FIELD);
    }

    public static Field getEntityQueryAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr.class.getDeclaredField(ENTITY_QUERY_ATT_FIELD);
    }

    public static String getSparqlQuery() {
        return QUERY;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", stringAttribute = " + stringAttribute;
        return out;
    }
}
