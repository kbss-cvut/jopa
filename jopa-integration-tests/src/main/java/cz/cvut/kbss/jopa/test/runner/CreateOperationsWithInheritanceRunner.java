package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;
import org.slf4j.Logger;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public abstract class CreateOperationsWithInheritanceRunner extends BaseRunner {

    // Mapped superclass
    private OWLClassQ entityQ;
    // Single inheritance - OWLClassT is a subclass of OWLClassS
    private OWLClassT entityT;

    protected CreateOperationsWithInheritanceRunner(Logger logger) {
        super(logger);
        init();
    }

    private void init() {
        this.entityQ = new OWLClassQ();
        entityQ.setStringAttribute("entityQStringAttribute");
        entityQ.setParentString("entityQParentStringAttribute");
        entityQ.setLabel("entityQLabel");
        entityQ.setOwlClassA(entityA);
        this.entityT = new OWLClassT();
        entityT.setName("entityT");
        entityT.setDescription("Description attribute is a part of the superclass.");
        entityT.setIntAttribute(Generators.randomInt(Integer.MAX_VALUE));
        entityT.setOwlClassA(entityA);
    }

    @Test
    public void testPersistEntityWithMappedSuperclass() {
        this.em = getEntityManager("PersistEntityWithMappedSuperclass", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityQ);
        em.getTransaction().commit();
        em.clear();

        assertNotNull(entityQ.getUri());
        final OWLClassQ result = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(result);
        assertEquals(entityQ.getStringAttribute(), result.getStringAttribute());
        assertEquals(entityQ.getParentString(), result.getParentString());
        assertEquals(entityQ.getLabel(), result.getLabel());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityQ.getOwlClassA().getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    public void testPersistEntityWithEntitySuperclass() {
        this.em = getEntityManager("PersistEntityWithEntitySuperclass", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityT);
        em.getTransaction().commit();
        em.clear();

        assertNotNull(entityT.getUri());
        final OWLClassT result = em.find(OWLClassT.class, entityT.getUri());
        assertNotNull(result);
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
        assertEquals(entityT.getIntAttribute(), result.getIntAttribute());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityA.getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }
}
