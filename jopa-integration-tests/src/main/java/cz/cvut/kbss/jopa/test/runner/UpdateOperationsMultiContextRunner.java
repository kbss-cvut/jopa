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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


public abstract class UpdateOperationsMultiContextRunner extends BaseRunner {

    public UpdateOperationsMultiContextRunner(Logger logger, PersistenceFactory persistenceFactory,
                                              DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testUpdateDataPropertyInContext() {
        this.em = getEntityManager("MultiUpdateDataPropertyInContext", false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        aDescriptor.addAttributeContext(fieldSpecification(OWLClassA.class, "stringAttribute"), CONTEXT_TWO);
        transactional(() -> em.persist(entityA, aDescriptor));

        final String newAttValue = "newStringAttributeValue";
        transactional(() -> {
            final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
            a.setStringAttribute(newAttValue);
        });

        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertEquals(newAttValue, resA.getStringAttribute());
        assertEquals(entityA.getTypes(), resA.getTypes());
    }

    @Test
    void testUpdateObjectPropertyToDifferentContext() {
        this.em = getEntityManager("MultiUpdateObjectPropertyToDifferent", false);
        final Descriptor dDescriptor = new EntityDescriptor();
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), aDescriptor);
        transactional(() -> {
            em.persist(entityD, dDescriptor);
            em.persist(entityA, aDescriptor);
        });

        final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(d.getOwlClassA());
        em.getTransaction().begin();
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontologies/newEntityA"));
        newA.setStringAttribute("newAStringAttribute");
        final Descriptor newADescriptor = new EntityDescriptor(CONTEXT_TWO);
        em.persist(newA, newADescriptor);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), newADescriptor);
        d.setOwlClassA(newA);
        em.getTransaction().commit();

        final OWLClassD resD = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
        assertEquals(newA.getStringAttribute(), resD.getOwlClassA().getStringAttribute());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
    }

    @Test
    void testUpdateAddToPropertiesInContext() {
        this.em = getEntityManager("MultiUpdateAddToPropertiesInContext", false);
        entityB.setProperties(Generators.createProperties());
        final Descriptor bDescriptor = new EntityDescriptor(CONTEXT_ONE);
        bDescriptor.addAttributeContext(em.getMetamodel().entity(OWLClassB.class).getProperties(), CONTEXT_TWO);
        transactional(() -> em.persist(entityB, bDescriptor));

        em.getTransaction().begin();
        final OWLClassB b = findRequired(OWLClassB.class, entityB.getUri(), bDescriptor);
        final String newKey = "http://krizik.felk.cvut.cz/jopa/ontologies/properties/newPropertyKey";
        final String newValue = "http://krizik.felk.cvut.cz/jopa/ontologies/newPropertyValue";
        final String newPropertyValue = "http://krizik.felk.cvut.cz/jopa/ontologies/NewValueOfAnOldProperty";
        final String propertyToChange = b.getProperties().keySet().iterator().next();
        b.getProperties().put(newKey, Collections.singleton(newValue));
        b.getProperties().get(propertyToChange).add(newPropertyValue);
        em.getTransaction().commit();

        final OWLClassB res = findRequired(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertTrue(TestEnvironmentUtils.arePropertiesEqual(b.getProperties(), res.getProperties()));
    }

    @Test
    void testUpdateAddToSimpleListInContext() {
        this.em = getEntityManager("MultiUpdateAddToSimpleListInContext", false);
        entityC.setSimpleList(Generators.createSimpleList(15));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final ObjectPropertyCollectionDescriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                fieldSpecification(OWLClassC.class, "simpleList"));
        cDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassC.class, "simpleList"), lstDescriptor);
        persistCWithLists(entityC, cDescriptor, lstDescriptor);

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        c.getSimpleList().add(entityA);
        em.persist(entityA, lstDescriptor);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
        boolean found = false;
        for (OWLClassA a : resC.getSimpleList()) {
            if (a.getUri().equals(entityA.getUri())) {
                assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
                assertEquals(entityA.getTypes(), a.getTypes());
                found = true;
                break;
            }
        }
        assertTrue(found);
    }

    private void persistCWithLists(OWLClassC instance, Descriptor cDescriptor,
                                   ObjectPropertyCollectionDescriptor listDescriptor) {
        transactional(() -> {
            em.persist(instance, cDescriptor);
            if (instance.getSimpleList() != null) {
                instance.getSimpleList().forEach(a -> em.persist(a, listDescriptor.getElementDescriptor()));
            }
            if (instance.getReferencedList() != null) {
                instance.getReferencedList().forEach(a -> em.persist(a, listDescriptor.getElementDescriptor()));
            }
        });
    }

    @Test
    void testUpdateAddToReferencedListInContext() {
        this.em = getEntityManager("MultiUpdateAddToReferencedListInContext", false);
        entityC.setReferencedList(Generators.createReferencedList(10));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final ObjectPropertyCollectionDescriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                fieldSpecification(OWLClassC.class, "referencedList"));
        cDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassC.class, "referencedList"), lstDescriptor);
        persistCWithLists(entityC, cDescriptor, lstDescriptor);

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        c.getReferencedList().add(entityA);
        em.persist(entityA, lstDescriptor);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
        boolean found = false;
        for (OWLClassA a : resC.getReferencedList()) {
            if (a.getUri().equals(entityA.getUri())) {
                assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
                assertEquals(entityA.getTypes(), a.getTypes());
                found = true;
                break;
            }
        }
        assertTrue(found);
    }

    @Test
    void testUpdateRemoveFromSimpleListInContext() {
        this.em = getEntityManager("MultiUpdateRemoveFromSimpleListInContext", false);
        entityC.setSimpleList(Generators.createSimpleList(15));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final ObjectPropertyCollectionDescriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                fieldSpecification(OWLClassC.class, "simpleList"));
        cDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassC.class, "simpleList"), lstDescriptor);
        persistCWithLists(entityC, cDescriptor, lstDescriptor);

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        final OWLClassA a = c.getSimpleList().get(0);
        c.getSimpleList().remove(0);
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
        assertNull(em.find(OWLClassA.class, a.getUri(), lstDescriptor));
    }

    @Test
    void testUpdateRemoveFromReferencedListInContext() {
        this.em = getEntityManager("MultiUpdateRemoveFromReferencedListInContext", false);
        entityC.setReferencedList(Generators.createReferencedList(10));
        final Descriptor cDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final ObjectPropertyCollectionDescriptor lstDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_TWO,
                fieldSpecification(OWLClassC.class, "referencedList"));
        cDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassC.class, "referencedList"), lstDescriptor);
        persistCWithLists(entityC, cDescriptor, lstDescriptor);

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        final List<OWLClassA> removed = new ArrayList<>();
        int i = 0;
        final Iterator<OWLClassA> it = c.getReferencedList().iterator();
        while (it.hasNext()) {
            i++;
            final OWLClassA a = it.next();
            if (i % 2 == 1) {
                continue;
            }
            removed.add(a);
            it.remove();
        }
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertEquals(entityC.getReferencedList().size() - removed.size(), resC.getReferencedList()
                .size());
        for (OWLClassA a : removed) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), lstDescriptor);
            assertNotNull(resA);
        }
    }

    @Test
    void testUpdatePlainIdentifierObjectPropertyValueInContext() {
        final Descriptor pDescriptor = new EntityDescriptor(CONTEXT_ONE);
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/originalIndividual"));
        this.em = getEntityManager("UpdatePlainIdentifierObjectPropertyValueInContext", true);
        transactional(() -> em.persist(entityP, pDescriptor));

        final OWLClassP toUpdate = findRequired(OWLClassP.class, entityP.getUri());
        em.detach(toUpdate);
        final URI newUri = URI.create("http://krizik.felk.cvut.cz/newIndividual");
        toUpdate.setIndividualUri(newUri);
        transactional(() -> em.merge(toUpdate));

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(newUri, res.getIndividualUri());
    }

    @Test
    void testUpdateFieldInMappedSuperclassInContext() {
        this.em = getEntityManager("UpdateFieldInMappedSuperclassInContext", true);
        final Descriptor qDescriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        qDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassQ.class, "owlClassA"), aDescriptor);
        em.getTransaction().begin();
        em.persist(entityQ, qDescriptor);
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();

        entityQ.setStringAttribute("newStringAttribute");
        entityQ.setParentString("newParentStringAttribute");
        entityQ.setLabel("newLabel");
        final OWLClassA newA = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA2"));
        newA.setStringAttribute("newAString");
        entityQ.setOwlClassA(newA);
        em.getTransaction().begin();
        em.merge(entityQ, qDescriptor);
        em.persist(newA, aDescriptor);
        em.getTransaction().commit();

        final OWLClassQ res = findRequired(OWLClassQ.class, entityQ.getUri(), qDescriptor);
        assertEquals(entityQ.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityQ.getParentString(), res.getParentString());
        assertEquals(entityQ.getLabel(), res.getLabel());
        assertNotNull(res.getOwlClassA());
        assertEquals(newA.getUri(), res.getOwlClassA().getUri());
        assertNotNull(em.find(OWLClassA.class, newA.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
    }

    @Test
    void testUpdateObjectPropertyInContextWithAssertionInContext() {
        this.em = getEntityManager("testUpdateObjectPropertyInContextWithAssertionInContext", true);
        final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE, false);
        final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
        dDescriptor.addAttributeDescriptor(fieldSpecification(OWLClassD.class, "owlClassA"), aDescriptor);
        transactional(() -> {
            em.persist(entityA, aDescriptor);
            em.persist(entityD, dDescriptor);
        });

        final OWLClassA newA = new OWLClassA(Generators.generateUri());
        newA.setStringAttribute("newString");
        transactional(() -> {
            final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
            d.setOwlClassA(newA);
            em.persist(newA, aDescriptor);
        });

        assertTrue(em.createNativeQuery("ASK { GRAPH ?g { ?s ?p ?o. }}", Boolean.class).setParameter("g", CONTEXT_TWO)
                .setParameter("s", entityD.getUri()).setParameter("p", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                .setParameter("o", newA.getUri()).getSingleResult());
        final OWLClassD result = findRequired(OWLClassD.class, entityD.getUri(), dDescriptor);
        assertNotNull(result.getOwlClassA());
        assertEquals(newA.getUri(), result.getOwlClassA().getUri());
    }

    @Test
    void updateSupportsObjectPropertyWithMultipleContexts() {
        this.em = getEntityManager("updateSupportsObjectPropertyWithMultipleContexts", true);
        final OWLClassF entityF = new OWLClassF(Generators.generateUri());
        final OWLClassA entityA2 = new OWLClassA(Generators.generateUri(), "another a");
        transactional(() -> {
            em.persist(entityF);
            em.persist(entityA, new EntityDescriptor(CONTEXT_ONE));
            em.persist(entityA2, new EntityDescriptor(CONTEXT_TWO));
        });

        entityF.setSimpleSet(new HashSet<>(Arrays.asList(entityA, entityA2)));
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_ONE)
                .addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_TWO);
        transactional(() -> em.merge(entityF, descriptor));

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri(), descriptor);
        assertEquals(2, result.getSimpleSet().size());
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA.getUri())));
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA2.getUri())));
    }

    @Test
    void updateWithinTransactionSupportsObjectPropertyWithMultipleContexts() {
        this.em = getEntityManager("updateSupportsObjectPropertyWithMultipleContexts", true);
        final OWLClassF entityF = new OWLClassF(Generators.generateUri());
        final OWLClassA entityA2 = new OWLClassA(Generators.generateUri(), "another a");
        transactional(() -> em.persist(entityF));

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_ONE)
                .addAttributeContext(fieldSpecification(OWLClassF.class, "simpleSet"), CONTEXT_TWO);
        transactional(() -> {
            final OWLClassF toUpdate = findRequired(OWLClassF.class, entityF.getUri(), descriptor);
            toUpdate.setSimpleSet(new HashSet<>(Arrays.asList(entityA, entityA2)));
            em.persist(entityA, new EntityDescriptor(CONTEXT_ONE));
            em.persist(entityA2, new EntityDescriptor(CONTEXT_TWO));
        });

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri(), descriptor);
        assertEquals(2, result.getSimpleSet().size());
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA.getUri())));
        assertTrue(result.getSimpleSet().stream().anyMatch(a -> a.getUri().equals(entityA2.getUri())));
    }
}
