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

import javax.lang.model.element.Element;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Field {
    private final List<String> imports = new ArrayList<>();
    private String name;
    private Type type;
    private String parentName;
    private List<MappingAnnotations> annotatedWith;

    public Field() {
        name = "";
        type = null;
        parentName = "";
        annotatedWith = new ArrayList<>();
    }

    public Field(Element elProperty, Element elParent) {
        this.name = elProperty.toString();
        this.type = new Type(elProperty.asType());
        this.parentName = elParent.toString();
        this.annotatedWith = Arrays.stream(MappingAnnotations.values())
                .filter(annotationEnum -> elProperty.getAnnotationMirrors().stream()
                        .anyMatch(annotationMirror ->
                                annotationMirror.getAnnotationType().toString()
                                        .contains(annotationEnum.getAnnotation())))
                .collect(Collectors.toList());
        if (type.getIsSimple()) {
            imports.add(type.getTypeName());
        } else {
            for (Type type : type.getTypes()) {
                imports.add(type.getTypeName());
                imports.add(this.type.getTypeName());
            }
        }
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

    public List<MappingAnnotations> getAnnotatedWith() {
        return annotatedWith;
    }

    public void setAnnotatedWith(List<MappingAnnotations> annotatedWith) {
        this.annotatedWith = annotatedWith;
    }

    public List<String> getImports() {
        return imports;
    }
}
