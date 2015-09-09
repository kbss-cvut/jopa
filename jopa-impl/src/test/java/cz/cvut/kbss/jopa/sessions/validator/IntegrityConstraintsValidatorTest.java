package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.sessions.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;

public class IntegrityConstraintsValidatorTest {

    private IntegrityConstraintsValidator validator = IntegrityConstraintsValidator.getValidator();

    @Test
    public void validationOfObjectWithoutConstraintsPasses() throws Exception {
        final OWLClassA obj = new OWLClassA();
        obj.setStringAttribute("aaaa");
        validator.validate(obj);
    }

    @Test
    public void validationOfObjectChangeSetWithValidChangesPasses() throws Exception {
        final OWLClassN clone = createInstanceWithMissingRequiredField();
        clone.setStringAttribute("newString");
        final OWLClassN orig = createInstanceWithMissingRequiredField();
        final ObjectChangeSet changeSet = new ObjectChangeSetImpl(orig, clone, null);
        changeSet.addChangeRecord(new ChangeRecordImpl(OWLClassN.getStringAttributeField().getName(), "newString"));

        validator.validate(changeSet);
    }

    @Test
    public void validationOfValidInstanceWithCardinalityConstraintsPasses() throws Exception {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSet(Collections.singleton(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        validator.validate(obj);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void missingRequiredAttributeOnObjectFailsValidation() throws Exception {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        validator.validate(n);
    }

    private OWLClassN createInstanceWithMissingRequiredField() {
        final OWLClassN n = new OWLClassN();
        n.setId("http://entityN");
        return n;
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void missingRequiredAttributeInChangeSetFailsValidation() throws Exception {
        final OWLClassN clone = createInstanceWithMissingRequiredField();
        final OWLClassN orig = createInstanceWithMissingRequiredField();
        final ObjectChangeSet changeSet = new ObjectChangeSetImpl(orig, clone, null);
        changeSet.addChangeRecord(new ChangeRecordImpl(OWLClassN.getStringAttributeField().getName(), null));

        validator.validate(changeSet);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void missingRequiredFieldValueFailsValidation() throws Exception {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        validator.validate(OWLClassN.getStringAttributeField(), n.getStringAttribute());
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void violatedMinimumCardinalityConstraintFailsValidation() throws Exception {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        validator.validate(obj);
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void violatedMaximumCardinalityConstraintFailsValidation() throws Exception {
        final OWLClassL orig = new OWLClassL();
        final OWLClassL clone = new OWLClassL();
        clone.setSimpleList(Collections.singletonList(new OWLClassA()));
        clone.setSet(Collections.singleton(new OWLClassA()));
        clone.setSingleA(new OWLClassA());
        clone.setReferencedList(new ArrayList<>());
        int max = OWLClassL.getReferencedListField().getAnnotation(ParticipationConstraints.class).value()[0].max();
        for (int i = 0; i < max + 1; i++) {
            clone.getReferencedList().add(new OWLClassA());
        }
        final ObjectChangeSet changeSet = new ObjectChangeSetImpl(orig, clone, null);
        changeSet.addChangeRecord(
                new ChangeRecordImpl(OWLClassL.getReferencedListField().getName(), clone.getReferencedList()));

        validator.validate(changeSet);
    }
}