package cz.cvut.kbss.jopa.modelgen.classmodel;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import java.util.ArrayList;
import java.util.List;

public class MetamodelClass {
    private String pckg;
    private String name;
    private String extend;
    private List<String> imports;
    private List<Field> fields;

    public MetamodelClass() {
        pckg = "";
        name = "";
        imports = new ArrayList<>();
        imports.add("cz.cvut.kbss.jopa.model.metamodel.*");
        imports.add("javax.annotation.processing.Generated");
        fields = new ArrayList<>();
    }

    public MetamodelClass(Element elClass) {
        String fullName = elClass.toString();
        boolean packag = fullName.contains(".");
        if (packag) {
            pckg = elClass.toString().substring(0, elClass.toString().lastIndexOf("."));
        } else {
            pckg = "";
        }
        name = elClass.getSimpleName().toString();
        extend = ((TypeElement) elClass).getSuperclass().toString();
        if (extend.equals(Object.class.getName())) extend = "";
        else if (extend.contains("<")) {
            extend = extend.substring(0, extend.indexOf("<"));
        }
        imports = new ArrayList<>();
        imports.add("cz.cvut.kbss.jopa.model.metamodel.*");
        imports.add("javax.annotation.processing.Generated");
        imports.add(fullName);
        fields = new ArrayList<>();
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

    public String getExtend() {
        return extend;
    }

    public void setExtend(String extend) {
        this.extend = extend;
    }

    public List<String> getImports() {
        return imports;
    }

    public void setImports(List<String> imports) {
        this.imports = imports;
    }

    public List<Field> getFields() {
        return fields;
    }

    public void addField(Field field) {
        this.fields.add(field);
        field.getImports().forEach((imbort) -> {
            if (!imports.contains(imbort)) {
                imports.add(imbort);
            }
        });
    }

    public void setFields(List<Field> properties) {
        this.fields = properties;
    }
}
