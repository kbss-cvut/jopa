package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RdfContainersTestRunner;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static cz.cvut.kbss.jopa.test.environment.util.ContainsSameEntities.containsSameEntities;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class RdfContainersTest extends RdfContainersTestRunner {

    private static final Logger LOG = org.slf4j.LoggerFactory.getLogger(RdfContainersTest.class);

    public RdfContainersTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    /*
    We cannot use verification queries because OWL2Query does not support COUNT, so we just use basic entity-level verification
     */

    @Test
    @Override
    public void persistPersistsEntityWithRdfContainerOfLiterals() {
        this.em = getEntityManager("persistPersistsEntityWithRdfContainerOfLiterals", false);
        final Set<Integer> levels = IntStream.range(0, 10).boxed().collect(Collectors.toSet());
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setLevels(levels);
        transactional(() -> em.persist(instance));

        final OWLClassR result = findRequired(OWLClassR.class, instance.getUri());
        assertNotNull(result);
        assertEquals(levels, result.getLevels());
    }

    @Test
    @Override
    public void updateUpdateEntityWithRdfContainerOfLiterals() {
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
    }

    @Test
    @Override
    public void updateUpdatesEntityWithRdfContainerOfEntities() {
        this.em = getEntityManager("updateUpdatesEntityWithRdfContainerOfEntities", true);
        final Set<OWLClassA> originalBag = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassA())
                                                    .collect(Collectors.toSet());
        entityC.setRdfBag(originalBag);
        transactional(() -> {
            originalBag.forEach(em::persist);
            em.persist(entityC);
        });

        entityC.setRdfBag(List.of(entityA));
        transactional(() -> {
            em.merge(entityC);
            em.persist(entityA);
        });

        final OWLClassC result = findRequired(OWLClassC.class, entityC.getUri());
        assertThat(result.getRdfBag(), containsSameEntities(List.of(entityA)));
    }

    @Test
    @Override
    public void persistPersistsEntityWithRdfContainerOfEntities() {
        this.em = getEntityManager("persistPersistsEntityWithRdfContainerOfEntities", false);
        final Set<OWLClassA> aBag = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassA())
                                             .collect(Collectors.toSet());
        entityC.setRdfBag(aBag);
        transactional(() -> {
            aBag.forEach(em::persist);
            em.persist(entityC);
        });

        final OWLClassC result = em.find(OWLClassC.class, entityC.getUri());
        assertEquals(aBag, result.getRdfBag());
    }
}
