package cz.cvut.kbss.jopa.modelgen.classmodel;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import java.util.ArrayList;
import java.util.List;

public class MetamodelClass {
    private String pckg;
    private String name;
    private String extend;
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
        this.extend = ((TypeElement) elClass).getSuperclass().toString();
        if (extend.equals(Object.class.getName())) extend = "";
        else if (extend.contains("<")) {
            this.extend = extend.substring(0, extend.indexOf("<"));
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

    public String getExtend() {
        return extend;
    }

    public void setExtend(String extend) {
        this.extend = extend;
    }

    public List<String> getImports() {
        return imports;
    }

    public List<Field> getFields() {
        return fields;
    }

    public void addField(Field field) {
        this.fields.add(field);
        field.getImports().stream().filter(imp -> !imports.contains(imp)).forEach(imports::add);
    }
}
