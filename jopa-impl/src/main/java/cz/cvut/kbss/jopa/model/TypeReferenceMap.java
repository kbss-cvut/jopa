package cz.cvut.kbss.jopa.model;

import java.util.*;

/**
 * Represents a map of references between types.
 * <p>
 * That is, it allows to get all entity classes which have attributes of the specified type.
 */
public class TypeReferenceMap {

    private final Map<Class<?>, Set<Class<?>>> referenceMap = new HashMap<>();

    /**
     * Registers reference relationships between the specified types.
     *
     * @param referencedType Type being referenced
     * @param referringType  Type referring to the referenced type
     */
    public void addReference(Class<?> referencedType, Class<?> referringType) {
        Objects.requireNonNull(referencedType);
        Objects.requireNonNull(referringType);
        if (!referenceMap.containsKey(referencedType)) {
            referenceMap.put(referencedType, new HashSet<>());
        }
        referenceMap.get(referencedType).add(referringType);
    }

    /**
     * Gets the set of entity classes containing an attribute of the specified type.
     *
     * @param referencedType Type being referenced for which referees should be returned
     * @return Set of referring classes, empty set if there are none
     */
    public Set<Class<?>> getReferringTypes(Class<?> referencedType) {
        return Collections.unmodifiableSet(referenceMap.getOrDefault(referencedType, Collections.emptySet()));
    }
}
