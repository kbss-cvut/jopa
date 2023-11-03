/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.lang.reflect.Field;
import java.net.URI;
import java.time.LocalDate;
import java.time.LocalDateTime;

@OWLClass(iri = Vocabulary.c_OwlClassT)
public class OWLClassT {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_T_LOCAL_DATE_ATTRIBUTE)
    private LocalDate localDate;

    @OWLDataProperty(iri = Vocabulary.P_T_LOCAL_DATETIME_ATTRIBUTE)
    private LocalDateTime localDateTime;

    @OWLObjectProperty(iri = Vocabulary.P_T_HAS_S)
    private OWLClassS owlClassS;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public LocalDate getLocalDate() {
        return localDate;
    }

    public void setLocalDate(LocalDate localDate) {
        this.localDate = localDate;
    }

    public LocalDateTime getLocalDateTime() {
        return localDateTime;
    }

    public void setLocalDateTime(LocalDateTime localDateTime) {
        this.localDateTime = localDateTime;
    }

    public OWLClassS getOwlClassS() {
        return owlClassS;
    }

    public void setOwlClassS(OWLClassS owlClassS) {
        this.owlClassS = owlClassS;
    }

    @Override
    public String toString() {
        return "OWLClassT{" +
                "uri=" + uri +
                ", localDate=" + localDate +
                ", localDateTime=" + localDateTime +
                '}';
    }

    public static String getClassIri() {
        return OWLClassT.class.getDeclaredAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws NoSuchFieldException {
        return OWLClassT.class.getDeclaredField("uri");
    }

    public static Field getLocalDateField() throws NoSuchFieldException {
        return OWLClassT.class.getDeclaredField("localDate");
    }

    public static Field getLocalDateTimeField() throws NoSuchFieldException {
        return OWLClassT.class.getDeclaredField("localDateTime");
    }

    public static Field getOwlClassSField() throws NoSuchFieldException {
        return OWLClassT.class.getDeclaredField("owlClassS");
    }
}
