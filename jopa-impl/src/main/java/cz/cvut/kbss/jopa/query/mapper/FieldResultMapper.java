package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

class FieldResultMapper {

    private final String variableName;

    private final FieldSpecification<?, ?> fieldSpec;

    FieldResultMapper(FieldResult fieldResult, EntityType<?> et) {
        this.variableName = fieldResult.variable();
        try {
            this.fieldSpec = et.getFieldSpecification(fieldResult.name());
        } catch (IllegalArgumentException e) {
            throw new SparqlResultMappingException("Invalid FieldResult mapping for field " + fieldResult.name() + ".",
                    e);
        }
    }

    /**
     * Creates mappers for which no explicit {@link FieldResult} configuration exists.
     * <p>
     * Variable name is taken to be the same as the name of the field.
     */
    FieldResultMapper(FieldSpecification<?, ?> fieldSpec) {
        this.fieldSpec = fieldSpec;
        this.variableName = fieldSpec.getName();
    }

    /**
     * Maps value from the specified result set to the specified target object's field based on the mapping represented
     * by this instance.
     *
     * @param resultSet Result set with value to map
     * @param target    Target object on which the field will be set
     */
    void map(ResultSet resultSet, Object target, UnitOfWork uow) {
        try {
            final Object value = resultSet.getObject(variableName);
            verifyValueRange(value);
            // This does currently only literal values, no references
            EntityPropertiesUtils.setFieldValue(fieldSpec.getJavaField(), target, value);
        } catch (OntoDriverException e) {
            throw new SparqlResultMappingException(e);
        }
    }

    private void verifyValueRange(Object value) {
        if (!fieldSpec.getJavaType().isAssignableFrom(value.getClass())) {
            throw new SparqlResultMappingException(
                    String.format("Value %s cannot be assigned to field of type %s.", value, fieldSpec.getJavaType()));
        }
    }
}
