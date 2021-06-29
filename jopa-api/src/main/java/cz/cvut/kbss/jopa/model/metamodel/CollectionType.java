package cz.cvut.kbss.jopa.model.metamodel;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

public enum CollectionType {
    SET(Set.class), LIST(List.class), COLLECTION(Collection.class), MAP(Map.class);

    private final Class<?> collectionClass;

    CollectionType(Class<?> cls) {
        this.collectionClass = cls;
    }

    public static CollectionType fromClass(Class<?> cls) {
        for (CollectionType type : values()) {
            if (type.collectionClass.isAssignableFrom(cls)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unsupported collection class " + cls);
    }
}
