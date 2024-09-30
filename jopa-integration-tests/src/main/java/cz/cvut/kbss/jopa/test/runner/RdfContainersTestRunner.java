package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class RdfContainersTestRunner extends BaseRunner {

    protected RdfContainersTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void persistPersistsEntityWithRdfContainerOfLiterals() {
        this.em = getEntityManager("persistPersistsEntityWithRdfContainerOfLiterals", false);
        final Set<Integer> levels = IntStream.range(0, 10).boxed().collect(Collectors.toSet());
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setLevels(levels);
        transactional(() -> em.persist(instance));

        final OWLClassR result = findRequired(OWLClassR.class, instance.getUri());
        assertNotNull(result);
        assertEquals(levels, result.getLevels());
        levels.forEach(level -> assertTrue(em.createNativeQuery("ASK {" +
                                                     "  ?owner ?hasContainer ?container . " +
                                                     "  ?container ?hasMember ?level ." +
                                                     "  FILTER (STRSTARTS(STR(?hasMember), \"" + RDF.NAMESPACE + "_\")) }", Boolean.class)
                                             .setParameter("owner", instance)
                                             .setParameter("hasContainer", URI.create(Vocabulary.P_R_HAS_RDF_ALT))
                                             .setParameter("level", level).getSingleResult()));
    }

    @Test
    void updateUpdateEntityWithRdfContainerOfLiterals() {
        this.em = getEntityManager("updateUpdateEntityWithRdfContainerOfLiterals", false);
        final Set<Integer> levels = IntStream.range(0, 10).boxed().collect(Collectors.toSet());
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setLevels(levels);
        transactional(() -> em.persist(instance));

        final Set<Integer> updated = IntStream.range(0, 10).filter(i -> i % 2 == 0).boxed().collect(Collectors.toSet());
        instance.setLevels(updated);
        transactional(() -> em.merge(instance));

        final OWLClassR result = findRequired(OWLClassR.class, instance.getUri());
        assertNotNull(result);
        assertEquals(updated, result.getLevels());
        assertEquals(updated.size(), em.createNativeQuery("SELECT (COUNT(?level) as ?cnt) WHERE {" +
                                               "  ?owner ?hasContainer ?container . " +
                                               "  ?container ?hasMember ?level ." +
                                               "  FILTER (STRSTARTS(STR(?hasMember), \"" + RDF.NAMESPACE + "_\")) }", Integer.class)
                                       .setParameter("owner", instance)
                                       .setParameter("hasContainer", URI.create(Vocabulary.P_R_HAS_RDF_ALT))
                                       .getSingleResult());
    }

    @Test
    void removeRemovesEntityWithRdfContainerOfLiterals() {
        this.em = getEntityManager("removeRemovesEntityWithRdfContainerOfLiterals", false);
        final Set<Integer> levels = IntStream.range(0, 10).boxed().collect(Collectors.toSet());
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setLevels(levels);
        transactional(() -> em.persist(instance));

        transactional(() -> em.remove(em.getReference(OWLClassR.class, instance.getUri())));

        assertNull(em.find(OWLClassR.class, instance.getUri()));
        assertFalse(em.createNativeQuery("ASK {" +
                             "  ?owner ?hasContainer ?container . " +
                             "  ?container ?hasMember ?elem ." +
                             "  FILTER (STRSTARTS(STR(?hasMember), \"" + RDF.NAMESPACE + ":_\")) }", Boolean.class)
                     .setParameter("owner", instance)
                     .setParameter("hasContainer", URI.create(Vocabulary.P_HAS_RDF_BAG)).getSingleResult());
    }

    @Test
    public void persistPersistsEntityWithRdfContainer() {
        this.em = getEntityManager("persistPersistsEntityWithRdfContainer", false);
        final Set<OWLClassA> aBag = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassA())
                                             .collect(Collectors.toSet());
        entityC.setRdfBag(aBag);
        transactional(() -> {
            aBag.forEach(em::persist);
            em.persist(entityC);
        });

        final OWLClassC result = em.find(OWLClassC.class, entityC.getUri());
        assertEquals(aBag, result.getRdfBag());
        aBag.forEach(a -> assertTrue(em.createNativeQuery("ASK {" +
                                               "  ?c ?hasContainer ?container . " +
                                               "  ?container ?hasMember ?a ." +
                                               "  FILTER (STRSTARTS(STR(?hasMember), \"" + RDF.NAMESPACE + ":_\")) }", Boolean.class)
                                       .setParameter("c", entityC)
                                       .setParameter("hasContainer", URI.create(Vocabulary.P_HAS_RDF_BAG))
                                       .setParameter("a", a).getSingleResult()));

    }
}