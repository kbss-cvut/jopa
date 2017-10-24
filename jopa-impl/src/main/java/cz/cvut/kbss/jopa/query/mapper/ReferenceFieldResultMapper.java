package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.ResultSet;

/**
 * Mapping SPARQL SELECT results to object property fields.
 * <p>
 * This means that the referenced instance has to be loaded.
 */
class ReferenceFieldResultMapper extends FieldResultMapper {

    ReferenceFieldResultMapper(FieldResult fieldResult, FieldSpecification<?, ?> fieldSpec) {
        super(fieldResult, fieldSpec);
    }

    ReferenceFieldResultMapper(FieldSpecification<?, ?> fieldSpec) {
        super(fieldSpec);
    }

    @Override
    void map(ResultSet resultSet, Object target, UnitOfWork uow) {
        final Object id = getVariableValue(resultSet);
        final Object value = uow.readObject(getFieldSpecification().getJavaType(), id, new EntityDescriptor());
        EntityPropertiesUtils.setFieldValue(getFieldSpecification().getJavaField(), target, value);
    }
}
