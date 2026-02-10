/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.modelgen.classmodel;

/**
 * JOPA mapping annotations.
 */
public enum MappingAnnotation {
    ID("cz.cvut.kbss.jopa.model.annotations.Id"),
    PROPERTIES("cz.cvut.kbss.jopa.model.annotations.Properties"),
    OBJECT_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty"),
    DATA_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLDataProperty"),
    ANNOTATION_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty"),
    TYPES("cz.cvut.kbss.jopa.model.annotations.Types");

    private final String annotation;

    MappingAnnotation(String annotation) {
        this.annotation = annotation;
    }

    public String getAnnotation() {
        return annotation;
    }
}
