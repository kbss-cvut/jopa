package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassS;
import cz.cvut.kbss.jopa.test.OWLClassSParent;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import static org.junit.Assert.*;

public abstract class PolymorphicSelectQueryRunner extends BaseQueryRunner {

    protected PolymorphicSelectQueryRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void selectLoadsInstanceOfMostConcreteSubclassOfAbstractEntity() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        final OWLClassSParent result =
                em.createNativeQuery("SELECT ?x WHERE { ?x ?hasName ?name . }", OWLClassSParent.class)
                  .setParameter("hasName", URI.create(CommonVocabulary.RDFS_LABEL))
                  .setParameter("name", t.getName(), "en").getSingleResult();
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes(t, (OWLClassT) result);
    }

    private void verifyEntityTAttributes(OWLClassT expected, OWLClassT actual) {
        assertEquals(expected.getUri(), actual.getUri());
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getIntAttribute(), actual.getIntAttribute());
        assertEquals(expected.getOwlClassA().getUri(), actual.getOwlClassA().getUri());
    }

    @Test
    public void selectLoadsInstanceOfMostConcreteSubclassOfConcreteEntity() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        final OWLClassS result =
                em.createNativeQuery("SELECT ?x WHERE { ?x ?hasName ?name . }", OWLClassS.class)
                  .setParameter("hasName", URI.create(CommonVocabulary.RDFS_LABEL))
                  .setParameter("name", t.getName(), "en").getSingleResult();
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes(t, (OWLClassT) result);
    }

    @Test
    public void selectByTypeLoadsAllIndividualsAsMostConcreteSubclassInstances() {
        final List<OWLClassT> data = QueryTestEnvironment.getData(OWLClassT.class);
        final EntityManager em = getEntityManager();
        // This will cause the type resolver to have to do some work
        em.getTransaction().begin();
        data.forEach(t -> {
            t.setTypes(new HashSet<>(Arrays.asList(Vocabulary.cOWLClassSParent, Vocabulary.cOWLClassS)));
            em.merge(t);
        });
        em.getTransaction().commit();
        final List<OWLClassSParent> result =
                em.createNativeQuery("SELECT ?x WHERE { ?x a ?type . }", OWLClassSParent.class)
                  .setParameter("type", URI.create(
                          Vocabulary.cOWLClassSParent)).getResultList();
        assertEquals(data.size(), result.size());

        boolean found;
        for (OWLClassT t : data) {
            found = false;
            for (OWLClassSParent tt : result) {
                if (t.getUri().equals(tt.getUri())) {
                    found = true;
                    assertTrue(tt instanceof OWLClassT);
                    verifyEntityTAttributes(t, (OWLClassT) tt);
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    public void selectLoadsInstanceAsGivenTypeWhenItIsConcreteAndFoundInTypesOfIndividual() {
        final OWLClassT t = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final EntityManager em = getEntityManager();
        em.getTransaction().begin();
        t.setTypes(Collections.singleton(Vocabulary.cOWLClassS));
        em.merge(t);
        em.getTransaction().commit();

        final OWLClassS result = em.createNativeQuery("SELECT ?x WHERE { ?x ?hasInt ?int. }", OWLClassS.class)
                                   .setParameter("hasInt", URI.create(Vocabulary.tIntegerAttribute))
                                   .setParameter("int", t.getIntAttribute()).getSingleResult();
        assertNotNull(result);
        assertFalse(result instanceof OWLClassT);
        assertEquals(t.getName(), result.getName());
        assertEquals(t.getDescription(), result.getDescription());
        assertTrue(result.getTypes().contains(Vocabulary.cOWLClassT));
    }
}
