package cz.cvut.kbss.jopa.modelgen.classmodel;

import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import java.util.*;

public class Type {
    private String typeName;
    private Boolean isSimple;
    private List<Type> types;

    public Type() {
        typeName = "";
        isSimple = true;
        types = new ArrayList<>();
    }

    public Type(TypeMirror tMirror) {
        if (isSimple(tMirror.toString())) {
            this.isSimple = true;
            this.types = null;
            TypeMirror upperBound = getUpperBound(tMirror);
            String fullName;
            fullName = Objects.requireNonNullElse(upperBound, tMirror).toString();
            if (fullName.contains("<")) {
                this.typeName = fullName.substring(0, fullName.indexOf("<"));
            } else {
                this.typeName = fullName;
            }
        } else {
            List<? extends TypeMirror> typeArgs = ((DeclaredType) tMirror).getTypeArguments();
            this.typeName = ((DeclaredType) tMirror).asElement().toString();
            this.isSimple = false;
            this.types = new ArrayList<>();
            typeArgs.forEach((typeMirror -> types.add(new Type(typeMirror))));
        }
    }

    private TypeMirror getUpperBound(TypeMirror type) {
        if (type instanceof TypeVariable) {
            return ((TypeVariable) type).getUpperBound();
        }
        return null;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public Boolean getIsSimple() {
        return isSimple;
    }

    public void setIsSimple(Boolean isSimple) {
        this.isSimple = isSimple;
    }

    public List<Type> getTypes() {
        return types;
    }

    public void setTypes(List<Type> types) {
        this.types = types;
    }

    private boolean isSimple(String name) {
        return !name.contains(Set.class.getName())
                && !name.contains(List.class.getName())
                && !name.contains(Stack.class.getName())
                && !name.contains(Map.class.getName());
    }
}
