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

import cz.cvut.kbss.jopa.modelgen.exception.ModelGenException;

import javax.lang.model.element.Element;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public class Field {
    private final List<String> imports = new ArrayList<>();
    private String name;
    private Type type;
    private String parentName;
    private MappingAnnotations annotatedWith;

    public Field() {
        name = "";
        parentName = "";
    }

    public Field(Element elProperty, Element elParent) {
        this.name = elProperty.toString();
        this.type = new Type(elProperty.asType());
        this.parentName = elParent.toString();
        this.annotatedWith = resolveFieldMappingAnnotation(elProperty).orElse(null);
        if (type.getIsSimple()) {
            imports.add(type.getTypeName());
        } else {
            for (Type type : type.getTypes()) {
                imports.add(type.getTypeName());
                imports.add(this.type.getTypeName());
            }
        }
    }

    private static Optional<MappingAnnotations> resolveFieldMappingAnnotation(Element field) {
        final List<MappingAnnotations> annotations = Arrays.stream(MappingAnnotations.values())
                                                           .filter(annotationEnum -> field.getAnnotationMirrors()
                                                                                          .stream()
                                                                                          .anyMatch(annotationMirror ->
                                                                                                  annotationMirror.getAnnotationType()
                                                                                                                  .toString()
                                                                                                                  .contains(annotationEnum.getAnnotation())))
                                                           .toList();
        if (annotations.size() > 1) {
            throw new ModelGenException("Field " + field + " is annotated by multiple mapping annotation.");
        }
        return annotations.isEmpty() ? Optional.empty() : Optional.of(annotations.get(0));
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getParentName() {
        return parentName;
    }

    public void setParentName(String parentName) {
        this.parentName = parentName;
    }

    public MappingAnnotations getAnnotatedWith() {
        return annotatedWith;
    }

    public void setAnnotatedWith(MappingAnnotations annotatedWith) {
        this.annotatedWith = annotatedWith;
    }

    /**
     * Checks if this field is annotated with the specified mapping annotation.
     *
     * @param annotation Annotation to check
     * @return {@code true} if this field is annotated with the specified annotation, {@code false} otherwise
     */
    public boolean isAnnotatedWith(MappingAnnotations annotation) {
        return Objects.equals(this.annotatedWith, annotation);
    }

    public List<String> getImports() {
        return imports;
    }
}
