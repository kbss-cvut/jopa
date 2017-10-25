package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;

import java.util.Optional;

/**
 * Mapping SPARQL SELECT results to object property fields.
 * <p>
 * This means that the referenced instance has to be loaded (unless the field is plain identifier).
 */
class ObjectPropertyFieldResultMapper extends FieldResultMapper {

    ObjectPropertyFieldResultMapper(FieldResult fieldResult, FieldSpecification<?, ?> fieldSpec) {
        super(fieldResult, fieldSpec);
    }

    ObjectPropertyFieldResultMapper(FieldSpecification<?, ?> fieldSpec) {
        super(fieldSpec);
    }

    @Override
    void map(ResultSet resultSet, Object target, UnitOfWork uow) {
        final Optional<Object> id = getVariableValue(resultSet);
        id.ifPresent(idValue -> {
            final Object value = resolveValue(uow, idValue);
            EntityPropertiesUtils.setFieldValue(getFieldSpecification().getJavaField(), target, value);
        });
    }

    private Object resolveValue(UnitOfWork uow, Object id) {
        if (IdentifierTransformer.isValidIdentifierType(getFieldSpecification().getJavaType())) {
            return IdentifierTransformer.transformToIdentifier(id, getFieldSpecification().getJavaType());
        }
        return uow.readObject(getFieldSpecification().getJavaType(), id, new EntityDescriptor());
    }
}
