package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.ResultSet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Maps result of a SPARQL SELECT query to an entity instance.
 */
class EntityResultMapper<T> implements SparqlResultMapper {

    private final EntityType<T> et;

    private final List<FieldResultMapper> fieldMappers = new ArrayList<>();

    EntityResultMapper(EntityType<T> et) {
        this.et = et;
    }

    void addFieldMapper(FieldResultMapper mapper) {
        fieldMappers.add(mapper);
    }

    List<FieldResultMapper> getFieldMappers() {
        return Collections.unmodifiableList(fieldMappers);
    }

    EntityType<T> getEntityType() {
        return et;
    }

    @Override
    public T map(ResultSet resultSet, UnitOfWork uow) {
        try {
            final T instance = et.getJavaType().newInstance();
            fieldMappers.forEach(m -> m.map(resultSet, instance, uow));
            return (T) uow.registerExistingObject(instance, new EntityDescriptor());
        } catch (InstantiationException | IllegalAccessException e) {
            // This is not expected, since an entity class must have a public no-arg constructor
            throw new SparqlResultMappingException(e);
        }
    }
}
