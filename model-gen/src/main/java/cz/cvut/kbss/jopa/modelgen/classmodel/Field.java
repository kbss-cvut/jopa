package cz.cvut.kbss.jopa.modelgen.classmodel;

import javax.lang.model.element.Element;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Field {
    private List<String> imports;
    private String name;
    private Type type;
    private String parentName;
    private List<String> annotatedWith;

    public Field(Element elProperty, Element elParent) {
        this.name = elProperty.toString();
        this.type = new Type(elProperty.asType());
        this.parentName = elParent.toString();
        this.annotatedWith = elProperty.getAnnotationMirrors().stream().map(prop -> prop.getAnnotationType().toString()).collect(Collectors.toList());
        imports = new ArrayList<>();
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

    public List<String> getAnnotatedWith() {
        return annotatedWith;
    }

    public void setAnnotatedWith(List<String> annotatedWith) {
        this.annotatedWith = annotatedWith;
    }

    public List<String> getImports() {
        return imports;
    }

    public void setImports(List<String> imports) {
        this.imports = imports;
    }
}
