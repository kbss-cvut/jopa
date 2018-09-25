/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

@SparqlResultSetMapping(name = OWLClassA.VARIABLE_MAPPING, variables = {
        @VariableResult(name = "x", type = URI.class),
        @VariableResult(name = "y", type = String.class)
})
@OWLClass(iri = Vocabulary.c_OwlClassA)
public class OWLClassA {

    public static final String VARIABLE_MAPPING = "OWLClassA.testMapping";

    private static final String TYPES_FIELD = "types";
    private static final String STR_ATT_FIELD = "stringAttribute";

    @Types(fetchType = FetchType.EAGER)
    private Set<String> types;

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_a_stringAttribute)
    private String stringAttribute;

    public OWLClassA() {
    }

    public OWLClassA(URI uri) {
        this.uri = uri;
    }

    public OWLClassA(URI uri, String stringAttribute) {
        this.uri = uri;
        this.stringAttribute = stringAttribute;
    }

    /**
     * Copy constructor
     */
    public OWLClassA(OWLClassA other) {
        this.uri = other.uri;
        this.stringAttribute = other.stringAttribute;
        this.types = new HashSet<>(other.types);
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

    @PostLoad
    public void postLoad() {
    }

    public static String getClassIri() {
        return OWLClassA.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
        return OWLClassA.class.getDeclaredField(STR_ATT_FIELD);
    }

    public static Field getTypesField() throws NoSuchFieldException, SecurityException {
        return OWLClassA.class.getDeclaredField(TYPES_FIELD);
    }

    public static Method getPostLoadCallback() throws NoSuchMethodException {
        return OWLClassA.class.getDeclaredMethod("postLoad");
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
}
