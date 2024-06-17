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
package cz.cvut.kbss.jopa.modelgen.classmodel;

import java.util.Arrays;
import java.util.List;

/**
 * JOPA mapping annotations.
 */
public enum MappingAnnotations {
    ID("cz.cvut.kbss.jopa.model.annotations.Id"),
    PROPERTIES("cz.cvut.kbss.jopa.model.annotations.Properties"),
    OBJECT_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty"),
    DATA_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLDataProperty"),
    ANNOTATION_PROPERTY("cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty"),
    TYPES("cz.cvut.kbss.jopa.model.annotations.Types");

    private final String annotation;

    MappingAnnotations(String annotation) {
        this.annotation = annotation;
    }

    public String getAnnotation() {
        return annotation;
    }

    public static List<MappingAnnotations> getAll() {
        return Arrays.asList(PROPERTIES, OBJECT_PROPERTY, DATA_PROPERTY, ANNOTATION_PROPERTY, TYPES, ID);
    }

}
