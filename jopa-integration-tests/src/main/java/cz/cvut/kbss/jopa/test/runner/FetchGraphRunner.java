package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassEE;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class FetchGraphRunner extends BaseRunner {

    public FetchGraphRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void selectWithFetchGraphSupportsPluralReferenceAttributes() {
        this.em = getEntityManager("selectWithFetchGraphSupportsPluralReferenceAttributes", false);
        final OWLClassEE instance = new OWLClassEE();
        instance.setLabel("Label");
        instance.seteInstances(IntStream.range(0, 3).mapToObj(i -> {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("String " + i);
            return e;
        }).collect(Collectors.toSet()));
        transactional(() -> {
            em.persist(instance);
            instance.geteInstances().forEach(em::persist);
        });

        final EntityGraph<OWLClassEE> fetchGraph = em.createEntityGraph(OWLClassEE.class);
        final Subgraph<OWLClassE> subgraph = fetchGraph.addSubgraph("eInstances");
        subgraph.addAttributeNodes("stringAttribute");
        final List<OWLClassEE> result = em.createQuery("SELECT e FROM " + OWLClassEE.class.getSimpleName() + " e", OWLClassEE.class)
                                          .setHint(QueryHints.FETCH_GRAPH, fetchGraph)
                                          .getResultList();
        assertEquals(1, result.size());
        final OWLClassEE eeResult = result.get(0);
        assertNull(eeResult.getLabel());
        assertEquals(instance.geteInstances().size(), eeResult.geteInstances().size());
        instance.geteInstances().forEach(e -> {
            final Optional<OWLClassE> eResult = eeResult.geteInstances().stream()
                                                        .filter(eeInstance -> eeInstance.getUri().equals(e.getUri()))
                                                        .findFirst();
            assertTrue(eResult.isPresent());
            assertEquals(e.getStringAttribute(), eResult.get().getStringAttribute());
        });
    }
}
