/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.*;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;


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
            em.persist(a, listDescriptor.getElementDescriptor());
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
            em.persist(a, listDescriptor.getElementDescriptor());
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
    void testRetrieveLazyReferenceFromContext() throws Exception {
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
        final Field refAField = OWLClassI.class.getDeclaredField("owlClassA");
        refAField.setAccessible(true);
        assertNull(refAField.get(resI));
        assertNotNull(resI.getOwlClassA());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri(), aDescriptor);
        // If we were using cache, ref.getOwlClassA() and resA would be the same
        assertEquals(resI.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
    }

    @Test
    void testRetrievePropertiesFromContext() {
        this.em = getEntityManager("MultiRetrievePropertiesFromContext", false);
        entityB.setProperties(Generators.createProperties(50));
        cOneDescriptor.addAttributeContext(em.getMetamodel().entity(OWLClassA.class).getProperties(), CONTEXT_TWO);
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
}
