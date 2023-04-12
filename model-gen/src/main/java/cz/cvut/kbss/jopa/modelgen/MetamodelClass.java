package cz.cvut.kbss.jopa.modelgen;

import javax.lang.model.element.Element;
import java.util.ArrayList;
import java.util.List;

public class MetamodelClass {
    private String paggage;
    private String name;
    private List<String> imports;
    private List<Property> properties;

    public MetamodelClass(Element elClass) {
        String fullName = elClass.toString();
        boolean packag = fullName.contains(".");
        if (packag) {
            paggage = elClass.toString().substring(0, elClass.toString().lastIndexOf("."));
        } else {
            paggage = "";
        }
        name = elClass.getSimpleName().toString();
        imports = new ArrayList<>();
        imports.add("cz.cvut.kbss.jopa.model.metamodel.*");
        imports.add(fullName);
        properties = new ArrayList<>();
    }

    public String getPaggage() {
        return paggage;
    }

    public void setPaggage(String paggage) {
        this.paggage = paggage;
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

    public List<Property> getProperties() {
        return properties;
    }

    public void addProperty(Property property) {
        this.properties.add(property);
        property.getImports().forEach((imbort) -> {
            if (!imports.contains(imbort)) {
                imports.add(imbort);
            }
        });
    }

    public void setProperties(List<Property> properties) {
        this.properties = properties;
    }
}
