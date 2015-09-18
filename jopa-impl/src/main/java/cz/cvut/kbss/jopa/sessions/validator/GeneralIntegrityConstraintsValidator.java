package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.ArrayList;
import java.util.List;

class GeneralIntegrityConstraintsValidator extends IntegrityConstraintsValidator {

    private final List<IntegrityConstraintsValidator> validators = new ArrayList<>();

    protected void addValidator(IntegrityConstraintsValidator validator) {
        validators.add(validator);
    }

    @Override
    public void validate(FieldSpecification<?, ?> attribute, Object attributeValue) {
        for (IntegrityConstraintsValidator validator : validators) {
            validator.validate(attribute, attributeValue);
        }
    }
}
