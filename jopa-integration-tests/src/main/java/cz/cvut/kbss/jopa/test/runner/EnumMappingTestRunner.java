/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.annotations.Individual;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.ObjectOneOfEnum;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public abstract class EnumMappingTestRunner extends BaseRunner {

    public EnumMappingTestRunner(Logger logger,
                                 PersistenceFactory persistenceFactory,
                                 DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void persistSupportsMappingEnumValuesToObjectOneOfIndividuals() throws Exception {
        this.em = getEntityManager("persistSupportsMappingEnumValuesToObjectOneOfIndividuals", false);
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setObjectOneOf(Generators.getRandomItem(Arrays.asList(ObjectOneOfEnum.values())));
        persist(instance);
        final String constantIri =
                ObjectOneOfEnum.class.getDeclaredField(instance.getObjectOneOf().name()).getAnnotation(
                        Individual.class).iri();
        verifyStatementsPresent(Collections.singletonList(
                new Quad(instance.getUri(), URI.create(Vocabulary.P_HAS_OBJECT_ONE_OF), URI.create(constantIri))), em);
    }

    @Test
    public void entityLifecycleSupportsMappingEnumValuesToObjectOneOfIndividuals() {
        this.em = getEntityManager("entityLifecycleSupportsMappingEnumValuesToObjectOneOfIndividuals", true);
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setObjectOneOf(ObjectOneOfEnum.ANNOTATION_PROPERTY);
        persist(instance);

        transactional(() -> {
            final OWLClassR toUpdate = findRequired(OWLClassR.class, instance.getUri());
            assertEquals(ObjectOneOfEnum.ANNOTATION_PROPERTY, toUpdate.getObjectOneOf());
            toUpdate.setObjectOneOf(ObjectOneOfEnum.DATATYPE_PROPERTY);
        });

        final OWLClassR result = findRequired(OWLClassR.class, instance.getUri());
        assertEquals(ObjectOneOfEnum.DATATYPE_PROPERTY, result.getObjectOneOf());
    }

    @Test
    public void persistSupportsOrdinalEnumMapping() throws Exception {
        this.em = getEntityManager("persistSupportsOrdinalEnumMapping", false);
        final OWLClassM instance = new OWLClassM();
        instance.initializeTestValues(true);
        persist(instance);

        verifyStatementsPresent(Collections.singletonList(
                new Quad(URI.create(instance.getKey()), URI.create(Vocabulary.p_m_ordinalEnumAttribute),
                         instance.getOrdinalEnumAttribute().ordinal())), em);
    }

    @Test
    public void entityLifecycleSupportsOrdinalEnumMapping() {
        this.em = getEntityManager("entityLifecycleSupportsOrdinalEnumMapping", true);
        final OWLClassM instance = new OWLClassM();
        instance.initializeTestValues(true);
        persist(instance);

        transactional(() -> {
            final OWLClassM toUpdate = findRequired(OWLClassM.class, instance.getKey());
            assertEquals(instance.getOrdinalEnumAttribute(), toUpdate.getOrdinalEnumAttribute());
            toUpdate.setOrdinalEnumAttribute(OWLClassM.Severity.HIGH);
        });

        final OWLClassM result = findRequired(OWLClassM.class, instance.getKey());
        assertEquals(OWLClassM.Severity.HIGH, result.getOrdinalEnumAttribute());
    }
}
