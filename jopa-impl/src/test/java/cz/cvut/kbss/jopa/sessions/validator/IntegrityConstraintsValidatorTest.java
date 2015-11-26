package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;

public class IntegrityConstraintsValidatorTest {

    private Metamodel metamodel;

    private IntegrityConstraintsValidator validator = IntegrityConstraintsValidator.getValidator();

    @Before
    public void setUp() throws Exception {
        final Configuration config = new Configuration(
                Collections.singletonMap(OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment"));
        this.metamodel = new MetamodelImpl(config, new EntityLoader());
    }

    @Test
    public void validationOfObjectWithoutConstraintsPasses() throws Exception {
        final OWLClassA obj = new OWLClassA();
        obj.setStringAttribute("aaaa");
        validator.validate(obj, metamodel.entity(OWLClassA.class), false);
    }

    @Test
    public void validationOfObjectChangeSetWithValidChangesPasses() throws Exception {
        final OWLClassN clone = createInstanceWithMissingRequiredField();
        clone.setStringAttribute("newString");
        final OWLClassN orig = createInstanceWithMissingRequiredField();
        final ObjectChangeSet changeSet = new ObjectChangeSetImpl(orig, clone, null);
        changeSet.addChangeRecord(new ChangeRecordImpl(OWLClassN.getStringAttributeField().getName(), "newString"));

        validator.validate(changeSet, metamodel);
    }

    @Test
    public void validationOfValidInstanceWithCardinalityConstraintsPasses() throws Exception {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSet(Collections.singleton(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        validator.validate(obj, metamodel.entity(OWLClassL.class), false);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void missingRequiredAttributeOnObjectFailsValidation() throws Exception {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        validator.validate(n, metamodel.entity(OWLClassN.class), false);
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

        validator.validate(changeSet, metamodel);
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void missingRequiredFieldValueFailsValidation() throws Exception {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        final Attribute<?, ?> att = metamodel.entity(OWLClassN.class)
                                             .getDeclaredAttribute(OWLClassN.getStringAttributeField().getName());
        validator.validate(att, n.getStringAttribute());
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void violatedMinimumCardinalityConstraintFailsValidation() throws Exception {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        validator.validate(obj, metamodel.entity(OWLClassL.class), false);
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

        validator.validate(changeSet, metamodel);
    }
}