package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
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
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class RdfContainersTestRunner extends BaseRunner {

    protected RdfContainersTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
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
