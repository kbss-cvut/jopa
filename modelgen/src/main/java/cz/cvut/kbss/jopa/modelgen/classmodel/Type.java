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

import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Type {
    // Full-qualified name
    private String typeName;
    // Simple name
    private String simpleName;
    private Boolean isSimple;
    private List<Type> types;

    public Type(String typeName, String simpleName) {
        this.typeName = typeName;
        this.simpleName = simpleName;
        isSimple = true;
        types = new ArrayList<>();
    }

    public Type(TypeMirror tMirror) {
        if (tMirror.getKind() != TypeKind.DECLARED && tMirror.getKind() != TypeKind.TYPEVAR) {
            throw new ModelGenException(
                    "Only declared types and type variables are supported, got " + tMirror + " of type " + tMirror.getKind());
        }
        if (isSimple(tMirror)) {
            this.isSimple = true;
            final DeclaredType declaredType = (DeclaredType) getUpperBound(tMirror);
            this.typeName = declaredType.asElement().toString();
            this.simpleName = declaredType.asElement().getSimpleName().toString();
        } else {
            assert tMirror instanceof DeclaredType;
            final DeclaredType declaredType = ((DeclaredType) tMirror);
            List<? extends TypeMirror> typeArgs = declaredType.getTypeArguments();
            this.typeName = declaredType.asElement().toString();
            this.simpleName = declaredType.asElement().getSimpleName().toString();
            this.isSimple = false;
            this.types = typeArgs.stream().map((Type::new)).toList();
        }
    }

    private static TypeMirror getUpperBound(TypeMirror type) {
        if (type instanceof TypeVariable) {
            return ((TypeVariable) type).getUpperBound();
        }
        return type;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public String getSimpleName() {
        return simpleName;
    }

    public void setSimpleName(String simpleName) {
        this.simpleName = simpleName;
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

    private static boolean isSimple(TypeMirror typeMirror) {
        final String typeName;
        if (typeMirror instanceof DeclaredType dt) {
            typeName = dt.asElement().toString();
        } else if (typeMirror instanceof TypeVariable tv) {
            typeName = tv.asElement().toString();
        } else {
            typeName = typeMirror.toString();
        }
        return !typeName.contains(Set.class.getName())
                && !typeName.contains(List.class.getName())
                && !typeName.contains(Collection.class.getName())
                && !typeName.contains(Map.class.getName());
    }

    @Override
    public String toString() {
        return simpleName + (isSimple ? "" : types);
    }
}
