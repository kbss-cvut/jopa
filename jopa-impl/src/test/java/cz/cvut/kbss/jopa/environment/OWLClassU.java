/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.PreUpdate;
import cz.cvut.kbss.jopa.vocabulary.DC;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.net.URI;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassU)
public class OWLClassU implements Serializable {

    @Id(generated = true)
    private URI id;

    @OWLDataProperty(iri = Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE)
    private MultilingualString singularStringAtt;

    @OWLDataProperty(iri = Vocabulary.P_U_PLURAL_MULTILINGUAL_ATTRIBUTE)
    private Set<MultilingualString> pluralStringAtt;

    @OWLDataProperty(iri = DC.Terms.MODIFIED)
    private LocalDateTime modified;

    public OWLClassU() {
    }

    public OWLClassU(URI id) {
        this.id = id;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public MultilingualString getSingularStringAtt() {
        return singularStringAtt;
    }

    public void setSingularStringAtt(MultilingualString singularStringAtt) {
        this.singularStringAtt = singularStringAtt;
    }

    public Set<MultilingualString> getPluralStringAtt() {
        return pluralStringAtt;
    }

    public void setPluralStringAtt(Set<MultilingualString> pluralStringAtt) {
        this.pluralStringAtt = pluralStringAtt;
    }

    public LocalDateTime getModified() {
        return modified;
    }

    public void setModified(LocalDateTime modified) {
        this.modified = modified;
    }

    @PreUpdate
    public void preUpdate() {
        this.modified = LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS);
    }

    @Override
    public String toString() {
        return "OWLClassU{" +
                "id=" + id +
                ", singularStringAtt=" + singularStringAtt +
                ", pluralStringAtt=" + pluralStringAtt +
                ", modified=" + modified +
                '}';
    }

    public static String getClassIri() {
        return OWLClassU.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getIdField() throws Exception {
        return OWLClassU.class.getDeclaredField("id");
    }

    public static Field getSingularStringAttField() throws Exception {
        return OWLClassU.class.getDeclaredField("singularStringAtt");
    }

    public static Field getPluralStringAttField() throws Exception {
        return OWLClassU.class.getDeclaredField("pluralStringAtt");
    }

    public static Field getModifiedField() throws Exception {
        return OWLClassU.class.getDeclaredField("modified");
    }
}
