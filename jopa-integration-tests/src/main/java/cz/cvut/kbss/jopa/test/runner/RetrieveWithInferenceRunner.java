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

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class RetrieveWithInferenceRunner extends BaseRunner {

    protected OWLClassW entityW = new OWLClassW();

    public RetrieveWithInferenceRunner(Logger logger, PersistenceFactory persistenceFactory,
                                       DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), hasItem(URI.create(Vocabulary.C_OWL_CLASS_A)));
    }

    @Test
    public void isInferredReturnsTrueForInferredPropertyValue() throws Exception {
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW w = findRequired(OWLClassW.class, entityW.getUri());
        assertTrue(em.isInferred(w, em.getMetamodel().entity(OWLClassW.class).getTypes(),
                                 URI.create(Vocabulary.C_OWL_CLASS_A)));
        assertFalse(em.isInferred(w, em.getMetamodel().entity(OWLClassW.class).getTypes(),
                                  URI.create(Vocabulary.C_OWL_CLASS_W)));
    }

    @Test
    public void findReturnsOnlyAssertedDataWhenDescriptorDisablesInference() throws Exception {
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW result =
                findRequired(OWLClassW.class, entityW.getUri(), new EntityDescriptor().disableInference());
        assertThat(result.getTypes(), not(hasItem(URI.create(Vocabulary.C_OWL_CLASS_A))));
    }

    @Test
    public void selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell() throws Exception {
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final OWLClassW result = em.createQuery("SELECT w FROM OWLClassW w WHERE w.uri = :uri", OWLClassW.class)
                .setParameter("uri", entityW.getUri())
                .setHint(QueryHints.DISABLE_INFERENCE, Boolean.TRUE.toString())
                .getSingleResult();
        assertThat(result.getTypes(), not(hasItem(URI.create(Vocabulary.C_OWL_CLASS_A))));
    }
}
