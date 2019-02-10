package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassL;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class PersistenceUnitUtilTest extends IntegrationTestBase {

    @Test
    void isLoadedReturnsTrueForNewlyRegisteredEntity() {
        final OWLClassA entityA = new OWLClassA(Generators.generateUri());
        em.persist(entityA);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA, "stringAttribute"));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entityA, "types"));
    }

    @Test
    void isLoadedReturnsTrueForLoadedExistingEntity() throws Exception {
        final URI uri = Generators.generateUri();
        initAxiomsForOWLClassA(NamedResource.create(uri),
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), false), "test");
        final OWLClassA entity = em.find(OWLClassA.class, uri);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "stringAttribute"));
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "types"));
    }

    @Test
    void isLoadedReturnsFalseForUnloadedLazilyLoadedAttribute() throws Exception {
        final URI uri = Generators.generateUri();
        initOwlClassLAxioms(uri);
        final OWLClassL entity = em.find(OWLClassL.class, uri);
        assertNotNull(entity);
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "singleA"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "set"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "referencedList"));
        assertFalse(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity, "simpleList"));
    }

    private void initOwlClassLAxioms(URI uri) throws Exception {
        final NamedResource subject = NamedResource.create(uri);
        final List<Axiom<?>> axioms = new ArrayList<>();
        final Axiom<?> classAssertion = new AxiomImpl<>(subject, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_L)));
        axioms.add(classAssertion);
        final AxiomDescriptor desc = new AxiomDescriptor(subject);
        desc.addAssertion(Assertion.createClassAssertion(false));
        when(connectionMock.find(desc)).thenReturn(axioms);
        when(connectionMock.contains(classAssertion, null)).thenReturn(true);
    }

    @Test
    void isLoadedReturnsTrueForEntityWhenAllAttributesAreUnloadedLazyAttributes() throws Exception {
        final URI uri = Generators.generateUri();
        initOwlClassLAxioms(uri);
        final OWLClassL entity = em.find(OWLClassL.class, uri);
        assertNotNull(entity);
        assertTrue(em.getEntityManagerFactory().getPersistenceUnitUtil().isLoaded(entity));
    }
}
