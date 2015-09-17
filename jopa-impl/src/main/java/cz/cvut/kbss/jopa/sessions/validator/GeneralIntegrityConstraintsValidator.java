package cz.cvut.kbss.jopa.sessions.validator;

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
}
