package cz.cvut.kbss.jopa.modelgen.classmodel;

import javax.lang.model.element.Element;
import java.util.ArrayList;
import java.util.List;

public class MetamodelClass {
    private String pckg;
    private String name;
    private List<String> imports;
    private List<Field> properties;

    public MetamodelClass(Element elClass) {
        String fullName = elClass.toString();
        boolean packag = fullName.contains(".");
        if (packag) {
            pckg = elClass.toString().substring(0, elClass.toString().lastIndexOf("."));
        } else {
            pckg = "";
        }
        name = elClass.getSimpleName().toString();
        imports = new ArrayList<>();
        imports.add("cz.cvut.kbss.jopa.model.metamodel.*");
        imports.add("javax.annotation.processing.Generated");
        imports.add(fullName);
        properties = new ArrayList<>();
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

    public List<String> getImports() {
        return imports;
    }

    public void setImports(List<String> imports) {
        this.imports = imports;
    }

    public List<Field> getProperties() {
        return properties;
    }

    public void addProperty(Field field) {
        this.properties.add(field);
        field.getImports().forEach((imbort) -> {
            if (!imports.contains(imbort)) {
                imports.add(imbort);
            }
        });
    }

    public void setProperties(List<Field> properties) {
        this.properties = properties;
    }
}
