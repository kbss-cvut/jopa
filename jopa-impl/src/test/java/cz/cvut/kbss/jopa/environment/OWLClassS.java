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

import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Set;

@EntityListeners(ParentListener.class)
@Inheritance(strategy = InheritanceType.TWO_STEP)
@OWLClass(iri = Vocabulary.c_OwlClassS)
public abstract class OWLClassS implements Serializable {

    @Id(generated = true)
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String name;

    @Types
    private Set<String> types;

    public OWLClassS() {
    }

    public OWLClassS(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Set<String> getTypes() {
        return types;
    }

    public void setTypes(Set<String> types) {
        this.types = types;
    }

    @SuppressWarnings("unused")
    @PrePersist
    private void prePersist() {
    }


    public static String getClassIri() {
        return OWLClassS.class.getDeclaredAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws NoSuchFieldException {
        return OWLClassS.class.getDeclaredField("uri");
    }

    public static Field getNameField() throws NoSuchFieldException {
        return OWLClassS.class.getDeclaredField("name");
    }

    public static Field getTypesField() throws NoSuchFieldException {
        return OWLClassS.class.getDeclaredField("types");
    }

    public static Method getPrePersistHook() throws Exception {
        return OWLClassS.class.getDeclaredMethod("prePersist");
    }
}
