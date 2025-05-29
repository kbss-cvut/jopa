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

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public abstract class CreateOperationsMultiContextRunner extends BaseRunner {

    private OWLClassF entityF;
    private OWLClassK entityK;

    private final EntityDescriptor cOneDescriptor = new EntityDescriptor(CONTEXT_ONE);
    private final EntityDescriptor cTwoDescriptor = new EntityDescriptor(CONTEXT_TWO);

    public CreateOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                              DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
        initialize();
    }

    private void initialize() {
        this.entityF = new OWLClassF();
        entityF.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityF"));
        this.entityK = new OWLClassK();
        entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityK"));
    }

    @Test
    void testPersistDataPropertyIntoContext() {
        this.em = getEntityManager("MultiPersistDataPropertyIntoContext", false);
        final Descriptor aDescriptor = new EntityDescriptor();
        aDescriptor.addAttributeContext(OWLClassA_.stringAttribute, CONTEXT_ONE);
        transactional(() -> em.persist(entityA, aDescriptor));

        final OWLClassA res = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityA.getTypes().size(), res.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
    }

    @Test
    void testPersistObjectPropertyIntoContext() {
        this.em = getEntityManager("MultiPersistObjectPropertyIntoContext", false);
        final Descriptor dDescriptor = new EntityDescriptor(false);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), cOneDescriptor);
        transactional(() -> {
            em.persist(entityD, dDescriptor);
            em.persist(entityA, cOneDescriptor);
        });

        final OWLClassD resD = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(resD.getOwlClassA());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
        assertEquals(resD.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
        assertTrue(resD.getOwlClassA().getTypes().containsAll(resA.getTypes()));
    }

    @Test
    void persistWithObjectPropertySavesAssertionIntoSubjectContextByDefault() {
        this.em = getEntityManager("persistWithObjectPropertySavesAssertionIntoSubjectContextByDefault", false);
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), cTwoDescriptor);
        transactional(() -> {
            em.persist(entityD, cOneDescriptor);
            em.persist(entityA, cTwoDescriptor);
        });

        final TypedQuery<Boolean> query = em.createNativeQuery("ASK {GRAPH ?g {?d ?hasA ?a .}}", Boolean.class)
                                            .setParameter("g", CONTEXT_ONE).setParameter("d", entityD.getUri())
                                            .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                                            .setParameter("a", entityA.getUri());
        assertTrue(query.getSingleResult());
        query.setParameter("g", CONTEXT_TWO);
        assertFalse(query.getSingleResult());
    }

    @Test
    void persistWithPluralObjectPropertySavesAssertionIntoSubjectContextByDefault() {
        this.em = getEntityManager("persistWithObjectPropertySavesAssertionIntoSubjectContextByDefault", false);
        final ObjectPropertyCollectionDescriptor opDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                fieldSpecification(OWLClassF.class, "simpleSet"));
        entityF.setSimpleSet(Collections.singleton(entityA));
        transactional(() -> em.persist(entityA, cTwoDescriptor));

        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassF.class, "simpleSet"), opDescriptor);
        transactional(() -> em.persist(entityF, cOneDescriptor));

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri());
        assertEquals(1, result.getSimpleSet().size());
        assertEquals(entityA.getUri(), result.getSimpleSet().iterator().next().getUri());

        final TypedQuery<Boolean> query = em.createNativeQuery("ASK {GRAPH ?g {?f ?hasA ?a .}}", Boolean.class)
                                            .setParameter("g", CONTEXT_ONE).setParameter("f", entityF.getUri())
                                            .setParameter("hasA", URI.create(Vocabulary.P_F_HAS_SIMPLE_SET))
                                            .setParameter("a", entityA.getUri());
        assertTrue(query.getSingleResult());
        query.setParameter("g", CONTEXT_TWO);
        assertFalse(query.getSingleResult());
    }

    @Test
    void testPersistWithGeneratedIntoContext() {
        this.em = getEntityManager("MultiPersistWithGeneratedIntoContext", false);
        transactional(() -> em.persist(entityE, cOneDescriptor));

        final OWLClassE res = findRequired(OWLClassE.class, entityE.getUri(), cOneDescriptor);
        assertEquals(entityE.getUri(), res.getUri());
        assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
    }

    @Test
    void testPersistReferenceIntoContextAndThenOwnerIntoDefault() {
        this.em = getEntityManager("ReferenceIntoContextThenOwnerIntoDefault", false);
        transactional(() -> em.persist(entityA, cOneDescriptor));

        em.clear();
        transactional(() -> {
            entityD.setOwlClassA(entityA);
            em.persist(entityD);
        });

        final OWLClassD res = findRequired(OWLClassD.class, entityD.getUri());
        assertNotNull(res.getOwlClassA());
    }

    @Test
    void persistTwiceIntoSameContextIsInvalid() {
        this.em = getEntityManager("MultiPersistTwiceIntoOneContext", false);
        transactional(() -> em.persist(entityA, cOneDescriptor));

        assertThrows(OWLEntityExistsException.class, () -> transactional(() -> em.persist(entityA, cOneDescriptor)));
    }

    @Test
    void persistTwiceIntoDifferentContextsIsLegal() {
        this.em = getEntityManager("MultiPersistTwiceIntoDifferentContexts", false);
        transactional(() -> em.persist(entityA, cOneDescriptor));

        transactional(() -> em.persist(entityA, cTwoDescriptor));


        final OWLClassA resOne = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        final OWLClassA resTwo = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        assertNotSame(resOne, resTwo);
        assertEquals(resOne.getUri(), resTwo.getUri());
        assertEquals(resOne.getStringAttribute(), resTwo.getStringAttribute());
    }

    @Test
    void testPersistPropertiesIntoDifferentContext() {
        this.em = getEntityManager("MultiPersistPropertiesIntoDifferentContext", false);
        final Descriptor bDescriptor = new EntityDescriptor();
        entityB.setProperties(Generators.createProperties(10));
        bDescriptor.addAttributeContext(em.getMetamodel().entity(OWLClassB.class).getProperties(), CONTEXT_ONE);
        transactional(() -> em.persist(entityB, bDescriptor));

        final OWLClassB res = findRequired(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityB.getProperties().size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : res.getProperties().entrySet()) {
            assertTrue(entityB.getProperties().containsKey(e.getKey()));
            assertEquals(e.getValue(), entityB.getProperties().get(e.getKey()));
        }
    }

    @Test
    void testPersistCascadeIntoThreeContexts() {
        this.em = getEntityManager("MultiPersistCascadeIntoThreeContexts", false);
        final Descriptor gDescriptor = new EntityDescriptor(false);
        final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        hDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassH.class, "owlClassA"), cTwoDescriptor);
        gDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassG.class, "owlClassH"), hDescriptor);
        transactional(() -> {
            em.persist(entityG, gDescriptor);
            assertTrue(em.contains(entityG));
            assertTrue(em.contains(entityH));
            assertTrue(em.contains(entityA));
        });

        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        final OWLClassH resH = findRequired(OWLClassH.class, entityH.getUri(), hDescriptor);
        assertSame(resA, resH.getOwlClassA());
        final OWLClassG resG = findRequired(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertSame(resH, resG.getOwlClassH());
    }

    @Test
    void persistCascadeIntoThreeContextSavesOPAssertionAlwaysIntoSubjectContextWhenConfigured() {
        this.em = getEntityManager(
                "persistCascadeIntoThreeContextSavesOPAssertionAlwaysIntoSubjectContextWhenConfigured", false);
        final Descriptor gDescriptor = new EntityDescriptor();
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassH.class, "owlClassA"), cTwoDescriptor);
        gDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassG.class, "owlClassH"), cOneDescriptor);
        transactional(() -> {
            em.persist(entityG, gDescriptor);
            assertTrue(em.contains(entityG));
            assertTrue(em.contains(entityH));
            assertTrue(em.contains(entityA));
        });

        assertTrue(em.createNativeQuery("ASK { ?s ?p ?o . }", Boolean.class)
                     .setParameter("s", entityG.getUri())
                     .setParameter("p", URI.create(Vocabulary.P_HAS_H))
                     .setParameter("o", entityH.getUri()).getSingleResult());
        final TypedQuery<Boolean> query = em.createNativeQuery("ASK {GRAPH ?g { ?s ?p ?o . }}", Boolean.class)
                                            .setParameter("g", CONTEXT_ONE)
                                            .setParameter("s", entityG.getUri())
                                            .setParameter("p", URI.create(Vocabulary.P_HAS_H))
                                            .setParameter("o", entityH.getUri());
        assertFalse(query.getSingleResult());
        query.setParameter("s", entityH.getUri())
             .setParameter("p", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
             .setParameter("o", entityA.getUri());
        assertTrue(query.getSingleResult());
        query.setParameter("g", CONTEXT_TWO);
        assertFalse(query.getSingleResult());
    }

    @Test
    void testPersistSetWithAttributeContexts() {
        this.em = getEntityManager("MultiPersistSetWithAttributeContexts", false);
        entityF.setSimpleSet(Generators.createSimpleSet(20));
        final Descriptor fDescriptor = new EntityDescriptor(false);
        final ObjectPropertyCollectionDescriptor setDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                fieldSpecification(OWLClassF.class, "simpleSet"), false);
        fDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassF.class, "simpleSet"), setDescriptor);
        setDescriptor.addAttributeContext(fieldSpecification(OWLClassA.class, "stringAttribute"), CONTEXT_TWO);
        setDescriptor.addAttributeContext(em.getMetamodel().entity(OWLClassA.class).getTypes(), CONTEXT_TWO);
        transactional(() -> {
            em.persist(entityF, fDescriptor);
            for (OWLClassA a : entityF.getSimpleSet()) {
                em.persist(a, setDescriptor.getElementDescriptor());
            }
        });

        final OWLClassF resF = findRequired(OWLClassF.class, entityF.getUri(), fDescriptor);
        assertEquals(entityF.getSimpleSet().size(), resF.getSimpleSet().size());
        for (OWLClassA a : resF.getSimpleSet()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri(), setDescriptor);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertEquals(a.getTypes(), resA.getTypes());
        }
    }

    @Test
    void testPersistEntityWithObjectPropertyWithGeneratedIdentifierAndPutTheReferenceIntoContext() {
        this.em = getEntityManager("PersistEntityWithObjectPropertyWithGeneratedIdentifierContexts", true);
        entityK.setOwlClassE(entityE);
        assertNull(entityE.getUri());
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassK.class, "owlClassE"), cTwoDescriptor);
        transactional(() -> {
            em.persist(entityK, cOneDescriptor);
            em.persist(entityE, cTwoDescriptor);
            assertNotNull(entityE.getUri());
        });

        final OWLClassE resE = findRequired(OWLClassE.class, entityE.getUri(), cTwoDescriptor);
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
        final OWLClassK resK = findRequired(OWLClassK.class, entityK.getUri(), cOneDescriptor);
        assertEquals(resE, resK.getOwlClassE());
    }

    @Test
    void testPersistEntityWithMappedSuperclassPuttingReferenceIntoDifferentContext() {
        this.em = getEntityManager("PersistEntityWithMappedSuperclassReferenceInContext", true);
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassQ.class, "owlClassA"), cTwoDescriptor);
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassQ.class, "owlClassA"), cTwoDescriptor);
        transactional(() -> {
            em.persist(entityQ, cOneDescriptor);
            em.persist(entityA, cTwoDescriptor);
        });

        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), cTwoDescriptor);
        final OWLClassQ resQ = findRequired(OWLClassQ.class, entityQ.getUri(), cOneDescriptor);
        assertEquals(entityQ.getStringAttribute(), resQ.getStringAttribute());
        assertEquals(entityQ.getParentString(), resQ.getParentString());
        assertEquals(entityQ.getLabel(), resQ.getLabel());
        assertNotNull(resQ.getOwlClassA());
        assertEquals(resA, resQ.getOwlClassA());
    }

    @Test
    void persistReferenceIntoContextBeforeOwnerDoesNotThrowUnpersistedChangeException() {
        this.em =
                getEntityManager("persistReferenceIntoContextBeforeOwnerDoesNotThrowUnpersistedChangeException", false);
        cOneDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"),
                new EntityDescriptor((URI) null));
        persist(entityA);
        transactional(() -> em.persist(entityD, cOneDescriptor));

        final OWLClassD result = findRequired(OWLClassD.class, entityD.getUri(), cOneDescriptor);
        assertNotNull(result.getOwlClassA());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
    }

    /**
     * Bug #58
     */
    @Test
    void persistEntityIntoContextAndAttributeIntoDefaultWorksCorrectly() {
        this.em = getEntityManager("persistEntityIntoContextAndAttributeIntoDefaultWorksCorrectly", false);
        cOneDescriptor.addAttributeContext(OWLClassA_.stringAttribute, null);
        transactional(() -> em.persist(entityA, cOneDescriptor));

        transactional(() -> {
            try {
                verifyStatementsPresent(Collections.singleton(
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                                entityA.getStringAttribute())), em);
                verifyStatementsNotPresent(Collections.singleton(
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                                entityA.getStringAttribute(), CONTEXT_ONE)), em);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Test
    void persistSupportsMultipleContextsSpecifiedForReferencedEntities() {
        this.em = getEntityManager("persistSupportsMultipleContextsSpecifiedForReferencedEntities", true);
        final OWLClassA entityA2 = new OWLClassA(Generators.generateUri(), "second instance");
        transactional(() -> {
            em.persist(entityA, cOneDescriptor);
            em.persist(entityA2, cTwoDescriptor);
        });

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_ONE)
                  .addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_TWO);
        entityF.setSimpleSet(new HashSet<>(Arrays.asList(entityA, entityA2)));
        transactional(() -> em.persist(entityF, descriptor));

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri(), descriptor);
        assertEquals(2, result.getSimpleSet().size());
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA.getUri())));
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA2.getUri())));
    }

    @Test
    void persistSupportsContextsDefinedByContextAnnotation() throws Exception {
        this.em = getEntityManager("persistSupportsContextsDefinedByContextAnnotation", true);
        final ClassInContext entity = new ClassInContext("Test entity");
        transactional(() -> em.persist(entity));

        verifyStatementsPresent(List.of(
                new Quad(entity.getId(), URI.create(RDF.TYPE), URI.create(ClassInContext.getClassIri()), URI.create("https://example.com/context")),
                new Quad(entity.getId(), URI.create(RDFS.LABEL), entity.getLabel(), TestEnvironment.PERSISTENCE_LANGUAGE, URI.create("https://example.com/context"))
        ), em);
    }
}
