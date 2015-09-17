package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

class GeneralIntegrityConstraintsValidator extends IntegrityConstraintsValidator {

    private final List<IntegrityConstraintsValidator> validators = new ArrayList<>();

    protected void addValidator(IntegrityConstraintsValidator validator) {
        validators.add(validator);
    }

    @Override
    public void validate(Field field, Object fieldValue) {
        for (IntegrityConstraintsValidator validator : validators) {
            validator.validate(field, fieldValue);
        }
    }

    @Override
    public void validate(Attribute<?, ?> attribute, Object attributeValue) {
        for (IntegrityConstraintsValidator validator : validators) {
            validator.validate(attribute, attributeValue);
        }
    }
}
