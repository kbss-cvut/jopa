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
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Testing implementation of Feature #121 - editable inferred attributes.
 */
public abstract class UpdateWithInferenceRunner extends BaseRunner {

    public UpdateWithInferenceRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void settingInferredAttributeFromNullToNewValueWorks() {
        assertNotNull(em);
        final OWLClassF entityF = new OWLClassF(Generators.generateUri());
        persist(entityF);

        final String updateValue = "updated value";
        transactional(() -> {
            final OWLClassF f = findRequired(OWLClassF.class, entityF.getUri());
            assertDoesNotThrow(() -> f.setSecondStringAttribute(updateValue));
        });

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri());
        assertEquals(updateValue, result.getSecondStringAttribute());
    }

    @Test
    public void settingSingularInferredAttributeFromOneAssertedValueToAnotherWorks() {
        assertNotNull(em);
        final OWLClassF entityF = new OWLClassF(Generators.generateUri());
        entityF.setSecondStringAttribute("Original value");
        persist(entityF);

        final String updateValue = "updated value";
        transactional(() -> {
            final OWLClassF f = findRequired(OWLClassF.class, entityF.getUri());
            assertDoesNotThrow(() -> f.setSecondStringAttribute(updateValue));
        });

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri());
        assertEquals(updateValue, result.getSecondStringAttribute());
    }

    @Test
    public void additiveChangeToAttributeWithInferredValuesWorks() throws Exception {
        assertNotNull(em);
        final OWLClassW entityW = new OWLClassW();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final URI newType = Generators.generateUri();
        transactional(() -> {
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertFalse(toUpdate.getTypes().isEmpty());
            toUpdate.getTypes().add(newType);
        });

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), hasItem(newType));
    }

    @Test
    public void removalOfAssertedValueOfInferredAttributeWorks() throws Exception {
        assertNotNull(em);
        final URI typeToRemove = Generators.generateUri();
        final OWLClassW entityW = new OWLClassW();
        entityW.setTypes(Collections.singleton(typeToRemove));
        final URI typeToAdd = Generators.generateUri();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        transactional(() -> {
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertThat(toUpdate.getTypes(), hasItem(typeToRemove));
            toUpdate.getTypes().remove(typeToRemove);
            toUpdate.getTypes().add(typeToAdd);
        });

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), hasItem(typeToAdd));
        assertThat(result.getTypes(), not(hasItem(typeToRemove)));
    }

    @Test
    public void removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException() throws Exception {
        assertNotNull(em);
        final URI typeToRemove = URI.create(Vocabulary.C_OWL_CLASS_A);
        final OWLClassW entityW = new OWLClassW();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                        typeToRemove)), em);
        persist(entityW);

        final OWLPersistenceException ex = assertThrows(OWLPersistenceException.class, () -> {
            em.getTransaction().begin();
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertThat(toUpdate.getTypes(), hasItem(typeToRemove));
            toUpdate.getTypes().remove(typeToRemove);
            em.getTransaction().commit();
        });
        // Depending on the change tracking strategy, the exception may be thrown immediately on change and being InferredAttributeException
        // Or on commit, in which case it will be a RollbackException with InferredAttributeModifiedException as cause
        if (!(ex instanceof InferredAttributeModifiedException)) {
            assertInstanceOf(InferredAttributeModifiedException.class, ex.getCause());
        }
    }

    @Test
    public void removalOfAssertedValueDoesNotAssertInferredValues() throws Exception {
        assertNotNull(em);
        final URI typeToRemove = Generators.generateUri();
        final URI inferredSupertype = Generators.generateUri();
        final OWLClassW entityW = new OWLClassW();
        entityW.setTypes(Collections.singleton(typeToRemove));
        persistTestData(Collections.singleton(new Quad(typeToRemove, URI.create(RDFS.SUB_CLASS_OF), inferredSupertype)),
                em);
        persist(entityW);

        final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(toUpdate.getTypes(), hasItems(typeToRemove, inferredSupertype));
        em.clear();
        toUpdate.getTypes().remove(typeToRemove);
        transactional(() -> em.merge(toUpdate));

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), not(hasItem(typeToRemove)));
        assertThat(result.getTypes(), not(hasItem(inferredSupertype)));
    }
}
