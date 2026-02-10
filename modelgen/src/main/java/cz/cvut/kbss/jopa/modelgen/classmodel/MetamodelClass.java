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

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import java.util.ArrayList;
import java.util.List;

public class MetamodelClass {
    private String pckg;
    private String name;
    private String classIri;
    private String superClass;
    private final List<String> imports;
    private final List<Field> fields = new ArrayList<>();

    public MetamodelClass() {
        this.pckg = "";
        this.name = "";
        imports = initDefaultImports();
    }

    public MetamodelClass(Element elClass) {
        String fullName = elClass.toString();
        boolean pckg = fullName.contains(".");
        if (pckg) {
            this.pckg = elClass.toString().substring(0, elClass.toString().lastIndexOf("."));
        } else {
            this.pckg = "";
        }
        this.name = elClass.getSimpleName().toString();
        this.superClass = ((TypeElement) elClass).getSuperclass().toString();
        if (superClass.equals(Object.class.getName())) {
            superClass = "";
        } else if (superClass.contains("<")) {
            this.superClass = superClass.substring(0, superClass.indexOf("<"));
        }
        this.imports = initDefaultImports();
        imports.add(fullName);
    }

    private static List<String> initDefaultImports() {
        final List<String> result = new ArrayList<>();
        result.add("cz.cvut.kbss.jopa.model.metamodel.*");
        result.add("javax.annotation.processing.Generated");
        return result;
    }

    public String getPckg() {
        return this.pckg;
    }

    public void setPckg(String pckg) {
        this.pckg = pckg;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getClassIri() {
        return classIri;
    }

    public void setClassIri(String classIri) {
        this.classIri = classIri;
    }

    public String getSuperClass() {
        return superClass;
    }

    public void setSuperClass(String superClass) {
        this.superClass = superClass;
    }

    public List<String> getImports() {
        return imports;
    }

    public List<Field> getFields() {
        return fields;
    }

    public boolean isEntityClass() {
        return classIri != null;
    }

    public void makeEntityClass(AnnotationMirror owlClassAnnotation) {
        this.classIri = owlClassAnnotation.getElementValues().entrySet().stream()
                                          .filter(e -> e.getKey().getSimpleName().contentEquals("iri"))
                                          .map(e -> e.getValue().toString()).findFirst().orElse(null);
    }

    public void addField(Field field) {
        this.fields.add(field);
        field.getImports().stream().filter(imp -> !imports.contains(imp)).forEach(imports::add);
    }
}
