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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.test.ClassInContext;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


public abstract class RetrieveOperationsMultiContextRunner extends BaseRunner {

    private final EntityDescriptor cOneDescriptor = new EntityDescriptor(CONTEXT_ONE);
    private final EntityDescriptor cTwoDescriptor = new EntityDescriptor(CONTEXT_TWO);

    public RetrieveOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                                DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testRetrieveSimilarFromTwoContexts() {
        this.em = getEntityManager("MultiRetrieveSimilarFromTwoContexts", false);
        final OWLClassA entityATwo = new OWLClassA();
        entityATwo.setUri(entityA.getUri());
        entityATwo.setStringAttribute("SomeCompletelyDifferentStringAttribute");
        transactional(() -> em.persist(entityA, cOneDescriptor));
        transactional(() -> em.persist(entityATwo, cTwoDescriptor));

        final OWLClassA resOne = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        assertEquals(entityA.getStringAttribute(), resOne.getStringAttribute());
        final OWLClassA resTwo = findRequired(OWLClassA.class, entityATwo.getUri(), cTwoDescriptor);
        assertEquals(entityATwo.getStringAttribute(), resTwo.getStringAttribute());
    }

    @Test
    void testRetrieveSimpleListFromContext() {
        this.em = getEntityManager("MultiRetrieveSimpleListFromContext", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        final Descriptor cDescriptor = new EntityDescriptor();
        final ListAttribute<OWLClassC, ?> simpleListAtt =
                em.getMetamodel().entity(OWLClassC.class).getDeclaredList("simpleList");
        final ObjectPropertyCollectionDescriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                simpleListAtt, false);
        cDescriptor.addAttributeDescriptor(simpleListAtt, listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getSimpleList()) {
            em.persist(a, listDescriptor.unwrap());
        }
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC.getSimpleList());
        assertEquals(entityC.getSimpleList().size(), resC.getSimpleList().size());
        for (OWLClassA a : entityC.getSimpleList()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri(), listDescriptor);
            assertEquals(a.getUri(), resA.getUri());
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
        }
    }

    @Test
    void testRetrieveReferencedListFromContext() {
        this.em = getEntityManager("MultiRetrieveReferencedListFromContext", false);
        entityC.setReferencedList(Generators.createReferencedList(15));
        final Descriptor cDescriptor = new EntityDescriptor();
        final ListAttribute<OWLClassC, ?> referencedListAtt =
                em.getMetamodel().entity(OWLClassC.class).getDeclaredList("referencedList");
        final ObjectPropertyCollectionDescriptor listDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
                referencedListAtt, false);
        cDescriptor.addAttributeDescriptor(referencedListAtt, listDescriptor);
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getReferencedList()) {
            em.persist(a, listDescriptor.unwrap());
        }
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(resC.getReferencedList());
        assertEquals(entityC.getReferencedList().size(), resC.getReferencedList().size());
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri(), listDescriptor);
            assertEquals(a.getUri(), resA.getUri());
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
        }
    }

    @Test
    void testRetrieveLazyReferenceFromContext() {
        this.em = getEntityManager("MultiRetrieveLazyReferenceFromContext", false);
        final Descriptor iDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        aDescriptor
                .addAttributeContext(em.getMetamodel().entity(OWLClassA.class).getDeclaredAttribute("stringAttribute"),
                        CONTEXT_ONE);
        iDescriptor.addAttributeDescriptor(em.getMetamodel().entity(OWLClassI.class).getDeclaredAttribute("owlClassA"),
                aDescriptor);
        em.getTransaction().begin();
        // The relationship is CascadeType.PERSIST
        em.persist(entityI, iDescriptor);
        em.getTransaction().commit();

        final OWLClassI resI = findRequired(OWLClassI.class, entityI.getUri(), iDescriptor);
        assertInstanceOf(LazyLoadingProxy.class, resI.getOwlClassA());
        // Trigger lazy loading
        assertNotNull(resI.getOwlClassA().getUri());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        // If we were using cache, ref.getOwlClassA() and resA would be the same
        assertEquals(resI.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
    }

    @Test
    void testRetrievePropertiesFromContext() {
        this.em = getEntityManager("MultiRetrievePropertiesFromContext", false);
        entityB.setProperties(Generators.createProperties(50));
        cOneDescriptor.addAttributeContext(em.getMetamodel().entity(OWLClassB.class).getProperties(), CONTEXT_TWO);
        cOneDescriptor
                .addAttributeContext(em.getMetamodel().entity(OWLClassB.class).getDeclaredAttribute("stringAttribute"),
                        null);
        em.getTransaction().begin();
        em.persist(entityB, cOneDescriptor);
        em.getTransaction().commit();

        final OWLClassB res = findRequired(OWLClassB.class, entityB.getUri(), cOneDescriptor);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(entityB.getProperties(), res.getProperties()));
    }

    @Test
    void retrieveSupportsRetrievalOfReferenceWherePropertyAssertionIsInSubjectContext() {
        this.em = getEntityManager("retrieveSupportsRetrievalOfReferenceWherePropertyAssertionIsInSubjectContext",
                false);
        cOneDescriptor
                .addAttributeDescriptor(em.getMetamodel().entity(OWLClassD.class).getDeclaredAttribute("owlClassA"),
                        cTwoDescriptor);
        transactional(() -> {
            em.persist(entityA, cTwoDescriptor);
            em.persist(entityD, cOneDescriptor);
        });

        final OWLClassD res = findRequired(OWLClassD.class, entityD.getUri(), cOneDescriptor);
        assertNotNull(res.getOwlClassA());
        assertEquals(entityA.getStringAttribute(), res.getOwlClassA().getStringAttribute());
    }

    /**
     * Bug #58
     */
    @Test
    void retrieveFromContextWithAttributeInDefaultWorksCorrectly() {
        this.em = getEntityManager("retrieveFromContextWithAttributeInDefaultWorksCorrectly", false);
        cOneDescriptor
                .addAttributeContext(em.getMetamodel().entity(OWLClassA.class).getDeclaredAttribute("stringAttribute"),
                        null);
        transactional(() -> {
            try {
                persistTestData(Arrays.asList(
                        new Quad(entityA.getUri(), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_A),
                                CONTEXT_ONE),
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                                entityA.getStringAttribute())), em);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });

        final OWLClassA result = findRequired(OWLClassA.class, entityA.getUri(), cOneDescriptor);
        assertNotNull(result.getStringAttribute());
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
    }

    @Test
    void retrieveSupportsSpecifyingMultipleContextsForObjectPropertyValues() {
        this.em = getEntityManager("retrieveSupportsSpecifyingMultipleContextsForObjectPropertyValues", false);
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_ONE)
                  .addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_TWO);
        final OWLClassA a2 = new OWLClassA(Generators.generateUri(), "string two");
        final URI ownerUri = Generators.generateUri();
        transactionalThrowing(() -> persistTestData(Arrays.asList(
                new Quad(entityA.getUri(), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_A),
                        CONTEXT_ONE),
                new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                        entityA.getStringAttribute(), CONTEXT_ONE),
                new Quad(a2.getUri(), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_A), CONTEXT_TWO),
                new Quad(a2.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), a2.getStringAttribute(),
                        CONTEXT_TWO),
                new Quad(ownerUri, URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_F)),
                new Quad(ownerUri, URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), entityA.getUri()),
                new Quad(ownerUri, URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), a2.getUri())), em));

        final OWLClassF result = findRequired(OWLClassF.class, ownerUri);
        assertEquals(2, result.getSimpleSet().size());
        final Optional<OWLClassA> aOneResult =
                result.getSimpleSet().stream().filter(a -> a.getUri().equals(entityA.getUri())).findAny();
        assertTrue(aOneResult.isPresent());
        assertEquals(entityA.getStringAttribute(), aOneResult.get().getStringAttribute());
        final Optional<OWLClassA> aTwoResult =
                result.getSimpleSet().stream().filter(a -> a.getUri().equals(a2.getUri())).findAny();
        assertTrue(aTwoResult.isPresent());
        assertEquals(a2.getStringAttribute(), aTwoResult.get().getStringAttribute());
    }

    @Test
    void retrieveSupportsSpecifyingMultipleContextsForEntity() {
        this.em = getEntityManager("retrieveSupportsSpecifyingMultipleContextsForEntity", false);
        final Descriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addContext(CONTEXT_TWO);
        final URI uri = URI.create(entityM.getKey());
        transactionalThrowing(() -> persistTestData(Arrays.asList(
                new Quad(uri, URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M), CONTEXT_ONE),
                new Quad(uri, URI.create(Vocabulary.p_m_booleanAttribute), entityM.getBooleanAttribute(), CONTEXT_ONE),
                new Quad(uri, URI.create(Vocabulary.p_m_dateAttribute), entityM.getDateAttribute(), CONTEXT_TWO)
        ), em));

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getBooleanAttribute(), result.getBooleanAttribute());
        assertEquals(entityM.getDateAttribute(), result.getDateAttribute());
    }

    @Test
    void getContextIdsSupportsContextIdRetrieval() {
        this.em = getEntityManager("getContextIdsSupportsContextIdRetrieval", false);
        final OWLClassA entityATwo = new OWLClassA();
        entityATwo.setUri(entityA.getUri());
        entityATwo.setStringAttribute("SomeCompletelyDifferentStringAttribute");
        transactional(() -> em.persist(entityA, cOneDescriptor));
        transactional(() -> em.persist(entityATwo, cTwoDescriptor));

        final List<URI> result = em.getContexts();
        assertEquals(2, result.size());
        assertThat(result, hasItems(CONTEXT_ONE, CONTEXT_TWO));
    }

    @Test
    void retrieveSupportsContextSpecifiedByAnnotation() {
        this.em = getEntityManager("retrieveSupportsContextSpecifiedByAnnotation", false);
        final ClassInContext entity = new ClassInContext("Test label");
        entity.setId(Generators.generateUri());
        transactionalThrowing(() -> persistTestData(List.of(
                new Quad(entity.getId(), URI.create(RDF.TYPE), URI.create(ClassInContext.getClassIri()), URI.create("https://example.com/context")),
                new Quad(entity.getId(), URI.create(RDFS.LABEL), entity.getLabel(), TestEnvironment.PERSISTENCE_LANGUAGE, URI.create("https://example.com/context"))
        ), em));

        final ClassInContext result = findRequired(ClassInContext.class, entity.getId());
        assertNotNull(result);
        assertEquals(entity.getLabel(), result.getLabel());
    }
}
