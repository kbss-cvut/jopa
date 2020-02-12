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

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassQ")
public class OWLClassQ extends QMappedSuperclass {

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#Q-stringAttribute")
    private String stringAttribute;

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public static String getClassIri() {
        return OWLClassQ.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("uri");
    }

    public static Field getStringAttributeField() throws Exception {
        return OWLClassQ.class.getDeclaredField("stringAttribute");
    }

    public static Field getLabelField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("label");
    }

    public static Field getParentStringField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("parentString");
    }

    public static Field getOwlClassAField() throws Exception {
        return QMappedSuperclass.class.getDeclaredField("owlClassA");
    }

    public static Set<Field> getPersistentFields() throws Exception {
        return new HashSet<>(
                Arrays.asList(getStringAttributeField(), getParentStringField(), getLabelField(), getOwlClassAField()));
    }
}
