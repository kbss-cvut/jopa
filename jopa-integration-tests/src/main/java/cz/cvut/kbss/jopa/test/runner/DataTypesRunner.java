/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassX;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public abstract class DataTypesRunner extends BaseRunner {

    private static final String VALUE = "https://example.com/query{?firstName,lastName}";

    public DataTypesRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void persistSupportsSavingValueOfAttributeWithExplicitDatatype() {
        this.em = getEntityManager("persistSupportsSavingValueOfAttributeWithExplicitDatatype", false);
        entityM.setExplicitDatatype(VALUE);
        transactional(() -> em.persist(entityM));

        verifyValueDatatype(URI.create(entityM.getKey()), Vocabulary.p_m_explicitDatatype,
                            TestEnvironment.EXPLICIT_DATATYPE);
    }

    @Test
    public void readSupportsValuesWithExplicitDatatype() throws Exception {
        this.em = getEntityManager("readSupportsValuesWithExplicitDatatype", false);
        persistTestData(Arrays.asList(
                new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_explicitDatatype),
                         new Literal(VALUE, TestEnvironment.EXPLICIT_DATATYPE))
        ), em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(VALUE, result.getExplicitDatatype());
    }

    @Test
    public void updatingExplicitDatatypeValuesIsSupported() {
        this.em = getEntityManager("updatingExplicitDatatypeValuesIsSupported", true);
        entityM.setExplicitDatatype(VALUE);
        persist(entityM);

        final String newValue = "https://example.com/query{?givenName,familyName}";
        transactional(() -> {
            final OWLClassM toUpdate = findRequired(OWLClassM.class, entityM.getKey());
            toUpdate.setExplicitDatatype(newValue);
        });

        final OWLClassM cachedResult = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(newValue, cachedResult.getExplicitDatatype());
        em.getEntityManagerFactory().getCache().evictAll();
        em.clear();

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(newValue, result.getExplicitDatatype());
    }

    @Test
    public void pluralAttributesWithExplicitDatatypeAreSupported() {
        this.em = getEntityManager("pluralAttributesWithExplicitDatatypeAreSupported", false);
        final OWLClassX entity = new OWLClassX();
        final Set<String> values = IntStream.range(0, 5)
                                            .mapToObj(Integer::toString)
                                            .collect(Collectors.toSet());
        entity.setExplicitDatatypes(values);

        persist(entity);

        final OWLClassX result = findRequired(OWLClassX.class, entity.getUri());
        assertEquals(values, result.getExplicitDatatypes());
    }

    @Test
    public void floatingPointAttributesSupportInfinityMapping() {
        this.em = getEntityManager("floatingPointAttributesSupportInfinity", false);
        entityM.setFloatAttribute(Float.NEGATIVE_INFINITY);
        entityM.setDoubleAttribute(Double.POSITIVE_INFINITY);
        persist(entityM);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(Float.NEGATIVE_INFINITY, result.getFloatAttribute());
        assertEquals(Double.POSITIVE_INFINITY, result.getDoubleAttribute());
    }
}
