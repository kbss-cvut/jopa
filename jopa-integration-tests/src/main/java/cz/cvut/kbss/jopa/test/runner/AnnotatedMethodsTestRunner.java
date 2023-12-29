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

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public abstract class AnnotatedMethodsTestRunner extends BaseRunner {
    protected OWLClassWithAnnotatedMethodsInInterfaceParent classWithAnnotatedMethodsInInterfaceParent;
    protected OWLClassWithUnProperties classWithUnProperties;

    public AnnotatedMethodsTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
        classWithUnProperties = new OWLClassWithUnProperties(Generators.generateUri());

        classWithAnnotatedMethodsInInterfaceParent = new OWLClassWithAnnotatedMethodsInInterfaceParent(Generators.generateUri());
        classWithAnnotatedMethodsInInterfaceParent.setData(new OWLClassWithUnProperties(Generators.generateUri()));
        classWithAnnotatedMethodsInInterfaceParent.setOrdinalEnumAttribute(OWLInterfaceE.Color.BLACK);
    }

    @Test
    void nonEmptyParticipationConstraintsInInterfaceThrowExceptionIfNotMet() {
        this.em = getEntityManager("nonEmptyParticipationConstraintsInInterfaceThrowExceptionIfNotMet", false);

        /// violate constraints
        classWithAnnotatedMethodsInInterfaceParent.setData(null);
        em.getTransaction().begin();

        em.persist(classWithAnnotatedMethodsInInterfaceParent);

        assertThrows(RollbackException.class, () -> em.getTransaction().commit());

    }

    @Test
    void nonEmptyParticipationConstraintsInInterfaceDoesNotThrowExceptionIfMet() {
        this.em = getEntityManager("nonEmptyParticipationConstraintsInInterfaceDoesNotThrowExceptionIfMet", false);

        em.getTransaction().begin();

        em.persist(classWithAnnotatedMethodsInInterfaceParent);

        em.getTransaction().commit();

        verifyExists(OWLClassWithAnnotatedMethodsInInterfaceParent.class, classWithAnnotatedMethodsInInterfaceParent.getUri());
    }


    @Test
    void maxParticipationConstraintsInInterfaceThrowExceptionIfNotMet() {
        this.em = getEntityManager("maxParticipationConstraintsInInterfaceThrowExceptionIfNotMet", false);

        Set<OWLClassWithUnProperties> dataList = new HashSet<>();
        dataList.add(new OWLClassWithUnProperties(Generators.generateUri()));
        dataList.add(new OWLClassWithUnProperties(Generators.generateUri()));
        dataList.add(new OWLClassWithUnProperties(Generators.generateUri()));
        classWithAnnotatedMethodsInInterfaceParent.setDataList(dataList);

        em.getTransaction().begin();

        em.persist(classWithAnnotatedMethodsInInterfaceParent);

        assertThrows(RollbackException.class, () -> em.getTransaction().commit());
    }

    @Test
    void maxParticipationConstraintsInInterfaceDoesNotThrowExceptionIfMet() {
        this.em = getEntityManager("maxParticipationConstraintsInInterfaceDoesNotThrowExceptionIfMet", false);

        Set<OWLClassWithUnProperties> dataList = new HashSet<>();
        dataList.add(new OWLClassWithUnProperties(Generators.generateUri()));
        dataList.add(new OWLClassWithUnProperties(Generators.generateUri()));
        classWithAnnotatedMethodsInInterfaceParent.setDataList(dataList);

        em.getTransaction().begin();

        em.persist(classWithAnnotatedMethodsInInterfaceParent);

        em.getTransaction().commit();

        verifyExists(OWLClassWithAnnotatedMethodsInInterfaceParent.class, classWithAnnotatedMethodsInInterfaceParent.getUri());
    }

    @Test
    void converterAnnotatedFieldsGetConverted() throws Exception {
        this.em = getEntityManager("converterAnnotatedFieldsGetConverted", false);

        classWithAnnotatedMethodsInInterfaceParent.setData(classWithUnProperties);

        final ZoneOffset value = ZoneOffset.ofHours(2);
        classWithAnnotatedMethodsInInterfaceParent.setWithConverter(value);

        persist(classWithAnnotatedMethodsInInterfaceParent);

        verifyStatementsPresent(Collections.singleton(
                new Quad(classWithAnnotatedMethodsInInterfaceParent.getUri(), URI.create(Vocabulary.p_m_withConverter),
                        classWithAnnotatedMethodsInInterfaceParent.getWithConverter().getId(), (String) null)), em);
    }

    @Test
    void enumeratedAnnotatedFieldsPersistedCorrectly() throws Exception {
            this.em = getEntityManager("enumeratedAnnotatedFieldsPersistedCorrectly", false);
            persist(classWithAnnotatedMethodsInInterfaceParent);

            verifyStatementsPresent(Collections.singletonList(
                    new Quad(classWithAnnotatedMethodsInInterfaceParent.getUri(), URI.create(Vocabulary.p_e_enumeratedOrdinalColor),
                            classWithAnnotatedMethodsInInterfaceParent.getOrdinalEnumAttribute().ordinal())), em);
        }


        @Test
    void sequenceAnnotationGetsUpdatedCorrectly(){
            this.em = getEntityManager("sequenceAnnotationGetsUpdatedCorrectly", true);
            classWithAnnotatedMethodsInInterfaceParent.setSimpleList(Generators.createListOfIdentifiers());

            persist(classWithAnnotatedMethodsInInterfaceParent);

            final OWLClassWithAnnotatedMethodsInInterfaceParent update = findRequired(OWLClassWithAnnotatedMethodsInInterfaceParent.class, classWithAnnotatedMethodsInInterfaceParent.getUri());

            em.getTransaction().begin();
            for (int i = 0; i < Generators.randomPositiveInt(2, 20); i++) {
                final URI u = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Added-" + i);
                // Insert at random position
                update.getSimpleList().add(Generators.randomInt(update.getSimpleList().size()), u);
            }
            em.getTransaction().commit();

            final OWLClassWithAnnotatedMethodsInInterfaceParent res = findRequired(OWLClassWithAnnotatedMethodsInInterfaceParent.class, classWithAnnotatedMethodsInInterfaceParent.getUri());
            assertEquals(update.getSimpleList(), res.getSimpleList());
        }


}
