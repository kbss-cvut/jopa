package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassK;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassP;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class ListsTestRunner extends BaseRunner {

    protected ListsTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    static void verifyLists(List<OWLClassE> expected, List<OWLClassE> actual) {
        assertEquals(expected.size(), actual.size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i).getUri(), actual.get(i).getUri());
            assertEquals(expected.get(i).getStringAttribute(), actual.get(i).getStringAttribute());
        }
    }

    @Test
    void testPersistWithSimpleList() {
        this.em = getEntityManager("PersistSimpleList", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        persistCWithLists(entityC);

        final OWLClassA a = findRequired(OWLClassA.class, entityC.getSimpleList().get(1).getUri());
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        assertNotNull(c.getSimpleList());
        assertFalse(c.getSimpleList().isEmpty());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertTrue(c.getSimpleList().contains(a));
    }

    @Test
    void persistingEntityWithSimpleListWithoutCascadeIsIllegal() {
        this.em = getEntityManager("PersistSimpleListNoCascade", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        assertThrows(RollbackException.class, () -> persist(entityC));
    }

    @Test
    void persistWithSimpleListSavesListReferenceWhenAllItemsArePersisted() {
        this.em = getEntityManager("persistWithSimpleListSavesListReferenceWhenAllItemsArePersisted", false);
        final OWLClassK entityK = new OWLClassK();
        entityK.setSimpleList(IntStream.range(0, 5).mapToObj(i -> {
            final OWLClassE item = new OWLClassE();
            item.setStringAttribute("item" + i);
            return item;
        }).collect(Collectors.toList()));
        em.getTransaction().begin();
        em.persist(entityK);
        entityK.getSimpleList().forEach(item -> assertNull(item.getUri()));
        entityK.getSimpleList().forEach(em::persist);
        entityK.getSimpleList().forEach(item -> assertNotNull(item.getUri()));
        em.getTransaction().commit();

        final OWLClassK result = findRequired(OWLClassK.class, entityK.getUri());
        verifyLists(entityK.getSimpleList(), result.getSimpleList());
    }

    @Test
    void testPersistWithReferencedList() {
        this.em = getEntityManager("PersistReferencedList", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        assertTrue(em.contains(entityC));
        assertTrue(em.contains(entityC.getReferencedList().get(0)));
        em.getTransaction().commit();

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        assertNotNull(c.getReferencedList());
        assertFalse(c.getReferencedList().isEmpty());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
            assertNotNull(resA);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertTrue(c.getReferencedList().contains(resA));
        }
    }


    @Test
    void persistingEntityWithReferencedListWithoutCascadeIsIllegal() {
        this.em = getEntityManager("PersistReferencedListNoCascade", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        assertThrows(RollbackException.class, () -> persist(entityC));
    }

    @Test
    void persistWithReferencedListSavesListReferenceWhenAllItemsArePersisted() {
        this.em = getEntityManager("persistWithReferencedListSavesListReferenceWhenAllItemsArePersisted", false);
        final OWLClassK entityK = new OWLClassK();
        entityK.setReferencedList(IntStream.range(0, 5).mapToObj(i -> {
            final OWLClassE item = new OWLClassE();
            item.setStringAttribute("item" + i);
            return item;
        }).collect(Collectors.toList()));
        em.getTransaction().begin();
        em.persist(entityK);
        entityK.getReferencedList().forEach(item -> assertNull(item.getUri()));
        entityK.getReferencedList().forEach(em::persist);
        entityK.getReferencedList().forEach(item -> assertNotNull(item.getUri()));
        em.getTransaction().commit();

        final OWLClassK result = findRequired(OWLClassK.class, entityK.getUri());
        verifyLists(entityK.getReferencedList(), result.getReferencedList());
    }

    @Test
    void testPersistSimpleAndReferencedList() {
        this.em = getEntityManager("PersistSimpleAndReferencedList", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        entityC.setSimpleList(Generators.createSimpleList(5));
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        assertNotNull(c.getSimpleList());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertNotNull(c.getReferencedList());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getSimpleList()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri());
            assertTrue(c.getSimpleList().contains(resA));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = findRequired(OWLClassA.class, a.getUri());
            assertTrue(c.getReferencedList().contains(resA));
        }
    }

    @Test
    void testPersistInstanceWithSimpleListOfIdentifiers() {
        this.em = getEntityManager("PersistInstanceWithSimpleListOfIdentifiers", false);
        entityP.setSimpleList(Generators.createListOfIdentifiers());
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getSimpleList(), res.getSimpleList());
    }

    @Test
    void testPersistInstanceWithReferencedListOfIdentifiers() {
        this.em = getEntityManager("PersistInstanceWithReferencedListOfIdentifiers", false);
        entityP.setReferencedList(Generators.createListOfIdentifiers());
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getReferencedList(), res.getReferencedList());
    }

    @Test
    void persistSupportsReferencedListsContainingDataPropertyLiteralValues() {
        this.em = getEntityManager("persistSupportsReferencedListsContainingDataPropertyLiteralValues", false);
        entityM.setLiteralReferencedList(Generators.createDataPropertyList());
        persist(entityM);

        for (int i = 0; i < entityM.getLiteralReferencedList().size(); i++) {
            assertTrue(em.createNativeQuery("ASK WHERE { ?prev ?hasNext ?node . ?node ?hasContent ?content . }", Boolean.class)
                         .setParameter("hasNext", URI.create(i == 0 ? Vocabulary.p_m_literalReferencedList : SequencesVocabulary.s_p_hasNext))
                         .setParameter("hasContent", URI.create(SequencesVocabulary.s_p_hasContents))
                         .setParameter("content", entityM.getLiteralReferencedList().get(i)).getSingleResult());
        }
    }

    @Test
    void testRemoveFromSimpleListAndRepository() {
        this.em = getEntityManager("RemoveFromSimpleList", false);
        final int size = 5;
        entityC.setSimpleList(Generators.createSimpleList(10));
        persistCWithLists(entityC);

        final int randIndex = Generators.randomInt(size);
        final OWLClassA a = findRequired(OWLClassA.class, entityC.getSimpleList().get(randIndex).getUri());
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        // We have to remove A from the simple list as well because otherwise we would break the chain in instances
        assertTrue(c.getSimpleList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertFalse(resC.getSimpleList().stream().anyMatch(item -> item.getUri().equals(a.getUri())));
    }

    @Test
    void testRemoveFromReferencedListAndRepository() {
        this.em = getEntityManager("RemoveFromReferencedList", false);
        final int size = 10;
        entityC.setReferencedList(Generators.createReferencedList(size));
        persistCWithLists(entityC);

        final int randIndex = Generators.randomInt(size);
        final OWLClassA a = findRequired(OWLClassA.class, entityC.getReferencedList().get(randIndex).getUri());
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        // We have to remove A from the referenced list as well because otherwise we would break the chain in instances
        assertTrue(c.getReferencedList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertFalse(resC.getReferencedList().stream().anyMatch(item -> item.getUri().equals(a.getUri())));
    }

    @Test
    void testRemoveListOwner() {
        this.em = getEntityManager("RemoveListOwner", false);
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        em.remove(c);
        em.getTransaction().commit();

        em.getEntityManagerFactory().getCache().evictAll();
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    void removingNewlyPersistedInstanceRemovesPendingListReferencesAndAllowsTransactionToFinish() {
        this.em = getEntityManager(
                "removingNewlyPersistedInstanceRemovesPendingListReferencesAndAllowsTransactionToFinish", true);
        em.getTransaction().begin();
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.persist(entityC);
        em.remove(entityC);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassC.class, entityC.getUri()));
        entityC.getSimpleList().forEach(a -> assertNull(em.find(OWLClassA.class, a.getUri())));
        entityC.getReferencedList().forEach(a -> assertNull(em.find(OWLClassA.class, a.getUri())));
    }

    @Test
    void removingListItemsFromNewlyPersistedOwnerRemovesThemFromPendingReferencesAndAllowsTransactionToFinish() {
        this.em = getEntityManager(
                "removingListItemsFromNewlyPersistedOwnerRemovesThemFromPendingReferencesAndAllowsTransactionToFinish",
                false);
        em.getTransaction().begin();
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.persist(entityC);
        entityC.getSimpleList().clear();
        entityC.getReferencedList().clear();
        em.getTransaction().commit();

        final OWLClassC result = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(result);
        assertNull(result.getSimpleList());
        assertNull(result.getReferencedList());
    }

    @Test
    void retrieveSupportsDataPropertyReferencedLists() throws Exception {
        this.em = getEntityManager("retrieveSupportsDataPropertyReferencedLists", false);
        final List<Quad> data = new ArrayList<>(List.of(new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M))));
        final List<LocalDate> dates = new ArrayList<>();
        URI previous = URI.create(entityM.getKey());
        for (int i = 5; i >= 0; i--) {
            final LocalDate d = LocalDate.now().minusDays(i);
            dates.add(d);
            final URI node = URI.create(entityM.getKey() + "-SEQ" + (5 - i));
            data.add(new Quad(previous, URI.create(i == 5 ? Vocabulary.p_m_literalReferencedList : SequencesVocabulary.s_p_hasNext), node));
            data.add(new Quad(node, SequencesVocabulary.p_hasContents, d));
            previous = node;
        }
        persistTestData(data, em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(dates, result.getLiteralReferencedList());
    }

    @Test
    void testRemoveFromSimpleList() {
        this.em = getEntityManager("UpdateRemoveFromSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        final OWLClassA a = c.getSimpleList().get(1);
        c.getSimpleList().remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = findRequired(OWLClassA.class, a.getUri());
        final OWLClassC resC = findRequired(OWLClassC.class, c.getUri());
        assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
        assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
        for (OWLClassA aa : resC.getSimpleList()) {
            assertNotEquals(resA.getUri(), aa.getUri());
        }
    }

    @Test
    void testAddToSimpleList() {
        this.em = getEntityManager("UpdateAddToSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.persist(entityA);
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri());
        assertFalse(c.getSimpleList().contains(a));
        c.getSimpleList().add(a);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
        assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri());
        assertTrue(resC.getSimpleList().contains(resA));
    }

    @Test
    void testClearSimpleList() {
        this.em = getEntityManager("UpdateClearSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        assertFalse(c.getSimpleList().isEmpty());
        em.getTransaction().begin();
        c.getSimpleList().clear();
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertTrue(resC.getSimpleList() == null || resC.getSimpleList().isEmpty());
        for (OWLClassA a : entityC.getSimpleList()) {
            verifyExists(OWLClassA.class, a.getUri());
        }
    }

    @Test
    void testReplaceSimpleList() {
        this.em = getEntityManager("UpdateReplaceSimpleList", true);
        entityC.setSimpleList(Generators.createSimpleList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        final List<OWLClassA> newList = new ArrayList<>(1);
        newList.add(entityA);
        em.getTransaction().begin();
        em.persist(entityA);
        c.setSimpleList(newList);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertEquals(newList.size(), resC.getSimpleList().size());
        boolean found;
        for (OWLClassA a : newList) {
            found = false;
            for (OWLClassA aa : resC.getSimpleList()) {
                if (a.getUri().equals(aa.getUri())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    void testRemoveFromReferencedList() {
        this.em = getEntityManager("UpdateRemoveFromReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.getTransaction().begin();
        final OWLClassA a = c.getReferencedList().get(Generators.randomInt(c.getReferencedList().size()));
        c.getReferencedList().remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = findRequired(OWLClassA.class, a.getUri());
        final OWLClassC resC = findRequired(OWLClassC.class, c.getUri());
        assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
        assertEquals(entityC.getReferencedList().size() - 1, resC.getReferencedList().size());
        for (OWLClassA aa : resC.getReferencedList()) {
            assertNotEquals(resA.getUri(), aa.getUri());
        }
    }

    @Test
    void testAddToReferencedList() {
        this.em = getEntityManager("UpdateAddToReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistCWithLists(entityC);

        em.getTransaction().begin();
        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        em.persist(entityA);
        c.getReferencedList().add(entityA);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
        assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
        final OWLClassA resA = findRequired(OWLClassA.class, entityA.getUri());
        assertTrue(resC.getReferencedList().contains(resA));
    }

    @Test
    void testClearReferencedList() {
        this.em = getEntityManager("UpdateClearReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        assertFalse(c.getReferencedList().isEmpty());
        em.getTransaction().begin();
        c.setReferencedList(null);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertNull(resC.getReferencedList());
        for (OWLClassA a : entityC.getReferencedList()) {
            verifyExists(OWLClassA.class, a.getUri());
        }
    }

    @Test
    void testReplaceReferencedList() {
        this.em = getEntityManager("UpdateReplaceReferencedList", true);
        entityC.setReferencedList(Generators.createReferencedList());
        persistCWithLists(entityC);

        final OWLClassC c = findRequired(OWLClassC.class, entityC.getUri());
        final List<OWLClassA> newList = new ArrayList<>(1);
        newList.add(entityA);
        em.getTransaction().begin();
        em.persist(entityA);
        c.setReferencedList(newList);
        em.getTransaction().commit();

        final OWLClassC resC = findRequired(OWLClassC.class, entityC.getUri());
        assertEquals(newList.size(), resC.getReferencedList().size());
        boolean found;
        for (OWLClassA a : newList) {
            found = false;
            for (OWLClassA aa : resC.getReferencedList()) {
                if (a.getUri().equals(aa.getUri())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            verifyExists(OWLClassA.class, a.getUri());
        }
    }

    @Test
    void testUpdateSimpleListOfIdentifiersByAddingNewItems() {
        this.em = getEntityManager("UpdateSimpleListOfIdentifiersByAddingItems", true);
        entityP.setSimpleList(Generators.createListOfIdentifiers());
        persist(entityP);

        final OWLClassP update = findRequired(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        for (int i = 0; i < Generators.randomPositiveInt(5, 10); i++) {
            final URI u = URI.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Added-" + i);
            // Insert at random position
            update.getSimpleList().add(Generators.randomInt(update.getSimpleList().size()), u);
        }
        em.getTransaction().commit();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(update.getSimpleList(), res.getSimpleList());
    }

    @Test
    void testUpdateReferencedListByRemovingAndAddingItems() {
        this.em = getEntityManager("UpdateReferencedListByRemovingAndAddingItems", true);
        entityP.setReferencedList(Generators.createListOfIdentifiers());
        persist(entityP);

        final OWLClassP update = findRequired(OWLClassP.class, entityP.getUri());
        em.getTransaction().begin();
        for (int i = 0; i < Generators.randomPositiveInt(5, 10); i++) {
            final URI u = URI.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Added-" + i);
            // We might even overwrite items set in previous iterations, but it does not matter. JOPA should handle it
            update.getReferencedList().set(Generators.randomInt(update.getReferencedList().size()), u);
        }
        em.getTransaction().commit();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(update.getReferencedList(), res.getReferencedList());
    }

    @Test
    void updateKeepsPendingListReferenceWhenItemIsAddedToIt() {
        this.em = getEntityManager("updateKeepsPendingListReferenceWhenItemIsAddedToIt", true);
        final OWLClassK entityK = new OWLClassK();
        entityK.setSimpleList(generateEInstances(5));
        entityK.setReferencedList(generateEInstances(6));
        em.getTransaction().begin();
        em.persist(entityK);
        entityK.getSimpleList().forEach(e -> {
            assertNull(e.getUri());
            em.persist(e);
        });
        entityK.getReferencedList().forEach(e -> {
            assertNull(e.getUri());
            em.persist(e);
        });
        em.getTransaction().commit();

        final OWLClassE addedSimple = new OWLClassE();
        addedSimple.setStringAttribute("addedSimple");
        final OWLClassE addedReferenced = new OWLClassE();
        addedReferenced.setStringAttribute("addedReferenced");
        em.getTransaction().begin();
        final OWLClassK update = em.find(OWLClassK.class, entityK.getUri());
        update.getSimpleList().add(addedSimple);
        update.getReferencedList().add(addedReferenced);
        assertNull(addedSimple.getUri());
        assertNull(addedReferenced.getUri());
        em.persist(addedSimple);
        em.persist(addedReferenced);
        em.getTransaction().commit();

        final OWLClassK result = findRequired(OWLClassK.class, entityK.getUri());
        assertEquals(addedSimple.getUri(), result.getSimpleList().get(result.getSimpleList().size() - 1).getUri());
        assertEquals(addedReferenced.getUri(),
                result.getReferencedList().get(result.getReferencedList().size() - 1).getUri());
    }

    private static List<OWLClassE> generateEInstances(int count) {
        return IntStream.range(0, count).mapToObj(i -> {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("instance" + i);
            return e;
        }).collect(Collectors.toList());
    }

    @Test
    void updateSupportsChangesInDataPropertyReferencedLists() {
        this.em = getEntityManager("updateSupportsChangesInDataPropertyReferencedLists", false);
        entityM.setLiteralReferencedList(Generators.createDataPropertyList());
        persist(entityM);

        final List<LocalDate> updatedList = new ArrayList<>(entityM.getLiteralReferencedList());
        updatedList.set(Generators.randomPositiveInt(0, updatedList.size()), LocalDate.now().minusDays(365));
        updatedList.add(LocalDate.now().plusDays(365));
        entityM.setLiteralReferencedList(updatedList);
        transactional(() -> em.merge(entityM));

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(updatedList, result.getLiteralReferencedList());
    }

    @Disabled
    @Test
    void persistSupportsMultilingualReferencedLists() {
        this.em = getEntityManager("persistSupportsMultilingualReferencedLists", false);
        entityM.setMultilingualReferencedList(List.of(
                new MultilingualString(Map.of("en", "First", "cs", "První")),
                new MultilingualString(Map.of("en", "Second", "cs", "Druhý")),
                new MultilingualString(Map.of("en", "Third", "cs", "Třetí"))
        ));
        persist(entityM);

        for (int i = 0; i < entityM.getMultilingualReferencedList().size(); i++) {
            final URI hasNextProperty = URI.create(i == 0 ? Vocabulary.p_m_literalReferencedList : SequencesVocabulary.s_p_hasNext);
            entityM.getMultilingualReferencedList().get(i).getValue()
                   .forEach((lang, value) -> assertTrue(em.createNativeQuery("ASK WHERE { ?prev ?hasNext ?node . ?node ?hasContent ?content . }", Boolean.class)
                                                          .setParameter("hasNext", hasNextProperty)
                                                          .setParameter("hasContent", URI.create(SequencesVocabulary.s_p_hasContents))
                                                          .setParameter("content", value, lang).getSingleResult()));
        }
    }
}
