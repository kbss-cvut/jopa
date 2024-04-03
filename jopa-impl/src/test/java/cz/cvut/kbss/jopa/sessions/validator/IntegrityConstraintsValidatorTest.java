/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Collections;

import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.isNotInferred;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.isNotLazy;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class IntegrityConstraintsValidatorTest {

    private MetamodelImpl metamodel;

    private final IntegrityConstraintsValidator validator = IntegrityConstraintsValidator.getValidator();

    @BeforeEach
    public void setUp() {
        final Configuration config = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment"));
        this.metamodel = new MetamodelImpl(config);
        metamodel.build(new PersistenceUnitClassFinder());
    }

    @Test
    public void validationOfObjectWithoutConstraintsPasses() {
        final OWLClassA obj = new OWLClassA();
        obj.setStringAttribute("aaaa");
        validator.validate(obj, metamodel.entity(OWLClassA.class));
    }

    @Test
    public void validationOfObjectChangeSetWithValidChangesPasses() throws Exception {
        final OWLClassN clone = createInstanceWithMissingRequiredField();
        clone.setStringAttribute("newString");
        final OWLClassN orig = createInstanceWithMissingRequiredField();
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(orig, clone, new EntityDescriptor());
        changeSet.addChangeRecord(new ChangeRecord(
                metamodel.entity(OWLClassN.class).getFieldSpecification(OWLClassN.getStringAttributeField().getName()),
                "newString"));

        validator.validate(changeSet, metamodel);
    }

    @Test
    public void validationOfValidInstanceWithCardinalityConstraintsPasses() {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSet(Collections.singleton(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        validator.validate(obj, metamodel.entity(OWLClassL.class));
    }

    @Test
    public void missingRequiredAttributeOnObjectFailsValidation() {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        assertThrows(IntegrityConstraintViolatedException.class,
                () -> validator.validate(n, metamodel.entity(OWLClassN.class)));
    }

    private OWLClassN createInstanceWithMissingRequiredField() {
        final OWLClassN n = new OWLClassN();
        n.setId("http://entityN");
        return n;
    }

    @Test
    public void missingRequiredAttributeInChangeSetFailsValidation() throws Exception {
        final OWLClassN clone = createInstanceWithMissingRequiredField();
        final OWLClassN orig = createInstanceWithMissingRequiredField();
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(orig, clone, new EntityDescriptor());
        changeSet.addChangeRecord(new ChangeRecord(
                metamodel.entity(OWLClassN.class).getFieldSpecification(OWLClassN.getStringAttributeField().getName()),
                null));

        assertThrows(IntegrityConstraintViolatedException.class, () -> validator.validate(changeSet, metamodel));
    }

    @Test
    public void missingRequiredFieldValueFailsValidation() throws Exception {
        final OWLClassN n = createInstanceWithMissingRequiredField();
        final Attribute<?, ?> att = metamodel.entity(OWLClassN.class)
                                             .getDeclaredAttribute(OWLClassN.getStringAttributeField().getName());
        assertThrows(IntegrityConstraintViolatedException.class,
                () -> validator.validate(n.getId(), att, n.getStringAttribute()));
    }

    @Test
    public void violatedMinimumCardinalityConstraintFailsValidation() {
        final OWLClassL obj = new OWLClassL();
        obj.setSimpleList(Collections.singletonList(new OWLClassA()));
        obj.setSingleA(new OWLClassA());

        assertThrows(CardinalityConstraintViolatedException.class,
                () -> validator.validate(obj, metamodel.entity(OWLClassL.class)));
    }

    @Test
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
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(orig, clone, new EntityDescriptor());
        changeSet.addChangeRecord(
                new ChangeRecord(metamodel.entity(OWLClassL.class)
                                          .getFieldSpecification(OWLClassL.getReferencedListField().getName()),
                        clone.getReferencedList()));

        assertThrows(CardinalityConstraintViolatedException.class, () -> validator.validate(changeSet, metamodel));
    }

    @Test
    public void validationDetectsICViolationsInMappedSuperclass() {
        final OWLClassQ q = new OWLClassQ();
        assertThrows(CardinalityConstraintViolatedException.class,
                () -> validator.validate(q, metamodel.entity(OWLClassQ.class)));
    }

    @Test
    public void violatedNonEmptyConstraintOnPluralAttributeFailsValidationOfObject() {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(Generators.createIndividualIdentifier());
        j.setOwlClassA(Collections.emptySet()); // This violates the nonEmpty constraint
        assertThrows(CardinalityConstraintViolatedException.class,
                () -> validator.validate(j, metamodel.entity(OWLClassJ.class)));
    }

    @Test
    public void violatedNonEmptyConstraintOnPluralAttributeFailsValidationOfChangeSet() throws Exception {
        final OWLClassJ original = new OWLClassJ(Generators.createIndividualIdentifier());
        original.setOwlClassA(Collections.singleton(new OWLClassA()));
        final OWLClassJ clone = new OWLClassJ(original.getUri());
        clone.setOwlClassA(Collections.emptySet());
        final ObjectChangeSet changeSet = ChangeSetFactory
                .createObjectChangeSet(original, clone, new EntityDescriptor());
        changeSet.addChangeRecord(new ChangeRecord(
                metamodel.entity(OWLClassJ.class).getFieldSpecification(OWLClassJ.getOwlClassAField().getName()),
                clone.getOwlClassA()));
        assertThrows(CardinalityConstraintViolatedException.class, () -> validator.validate(changeSet, metamodel));
    }

    @Test
    void validationSkipsLazyFieldsWhenConfiguredTo() {
        final OWLClassJ instance = new OWLClassJ(Generators.createIndividualIdentifier());
        assertDoesNotThrow(() -> validator.validate(instance, metamodel.entity(OWLClassJ.class), isNotLazy()));
    }

    @Test
    void validationSkipsInferredFieldsWhenConfiguredTo() throws Exception {
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        instance.setStringAttribute(null);
        final EntityType<OWLClassA> et = mock(EntityType.class);
        final Identifier idAtt = mock(Identifier.class);
        when(et.getIdentifier()).thenReturn(idAtt);
        when(idAtt.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
        final Attribute<OWLClassA, String> strAtt = mock(Attribute.class);
        when(strAtt.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        when(et.getFieldSpecifications()).thenReturn(Collections.singleton(strAtt));
        when(strAtt.isInferred()).thenReturn(true);
        final ParticipationConstraint pc = new ParticipationConstraint() {

            @Override
            public Class<? extends Annotation> annotationType() {
                return ParticipationConstraint.class;
            }

            @Override
            public String owlObjectIRI() {
                return Vocabulary.p_a_stringAttribute;
            }

            @Override
            public int min() {
                return 1;
            }

            @Override
            public int max() {
                return 1;
            }
        };
        when(strAtt.getConstraints()).thenReturn(new ParticipationConstraint[]{pc});
        assertDoesNotThrow(() -> validator.validate(instance, et, isNotInferred()));
    }
}
