package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Minimal interface for mapping entity classes to the corresponding metamodel objects.
 */
public interface MetamodelClassMapper {

    /**
     * Maps the specified entity class to the corresponding metamodel object.
     *
     * @param cls Class to map
     * @param <X> Class type
     * @return Entity type
     */
    <X> AbstractIdentifiableType<X> entity(Class<X> cls);
}
