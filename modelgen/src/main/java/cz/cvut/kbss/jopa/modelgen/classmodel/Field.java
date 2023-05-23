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
            if (!type.getTypeName().contains(" ")) {
                imports.add(type.getTypeName());
            } else {
                imports.add(type.getTypeName().substring(type.getTypeName().lastIndexOf(" ") + 1));
            }
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
