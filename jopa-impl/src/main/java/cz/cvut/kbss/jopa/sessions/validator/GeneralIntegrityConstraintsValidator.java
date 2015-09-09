package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.Basic;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class GeneralIntegrityConstraintsValidator extends IntegrityConstraintsValidator {

    private final List<IntegrityConstraintsValidator> validators = new ArrayList<>();

    protected void addValidator(IntegrityConstraintsValidator validator) {
        validators.add(validator);
    }

    @Override
    public void validate(Field field, Object fieldValue) {
        validateBasic(field, fieldValue);
        for (IntegrityConstraintsValidator validator : validators) {
            validator.validate(field, fieldValue);
        }
    }

    private void validateBasic(Field field, Object fieldValue) {
        final Basic basic = field.getAnnotation(Basic.class);
        if (basic != null) {
            if (!basic.optional() && fieldValue == null && !Collection.class.isAssignableFrom(field.getType())) {
                throw new IntegrityConstraintViolatedException("Missing required value of field " + field);
            }
        }
    }
}
