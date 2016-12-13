package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;
import org.slf4j.Logger;

import static org.hamcrest.core.Is.isA;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public abstract class UpdateOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public UpdateOperationsWithInheritanceRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testUpdateFieldsOfEntityWithMappedSuperclass() {
        this.em = getEntityManager("UpdateEntityWithMappedSuperclass", true);
        persist(entityQ, entityA);

        entityQ.setStringAttribute("newStringAttribute");
        entityQ.setParentString("newParentStringAttribute");
        entityQ.setLabel("newLabel");
        em.getTransaction().begin();
        em.merge(entityQ);
        em.getTransaction().commit();

        final OWLClassQ res = em.find(OWLClassQ.class, entityQ.getUri());
        assertEquals(entityQ.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityQ.getParentString(), res.getParentString());
        assertEquals(entityQ.getLabel(), res.getLabel());
    }

    @Test
    public void testUpdateObjectPropertyInMappedSuperclass() {
        this.em = getEntityManager("UpdateObjectPropertyInMappedSuperclass", true);
        persist(entityQ, entityA);
        final OWLClassA entityA2 = new OWLClassA();
        entityA2.setStringAttribute("entityA2StringAttribute");

        entityQ.setOwlClassA(entityA2);
        em.getTransaction().begin();
        em.merge(entityQ);
        em.persist(entityA2);
        em.getTransaction().commit();

        final OWLClassQ res = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(res.getOwlClassA());
        assertEquals(entityA2.getUri(), res.getOwlClassA().getUri());
        assertEquals(entityA2.getStringAttribute(), res.getOwlClassA().getStringAttribute());
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void settingNonEmptyFieldInMappedSuperclassThrowsICViolationOnMerge() {
        this.em = getEntityManager("SettingNonEmptyFieldInMappedSuperclassThrowsICViolation", true);
        persist(entityQ, entityA);

        thrown.expect(RollbackException.class);
        thrown.expectCause(isA(IntegrityConstraintViolatedException.class));

        entityQ.setOwlClassA(null);
        em.getTransaction().begin();
        em.merge(entityQ);
        em.getTransaction().commit();
    }

    @Test
    public void testUpdateDataPropertyInEntitySuperclass() {
        this.em = getEntityManager("updateDataPropertyInEntitySuperclass", true);
        persist(entityT, entityA);

        final String newName = "newName";
        final int newInt = Generators.randomInt(Integer.MAX_VALUE);
        entityT.setName(newName);
        entityT.setIntAttribute(newInt);
        final String newDescription = "new entity description";
        em.getTransaction().begin();
        em.merge(entityT);
        entityT.setDescription(newDescription);
        em.getTransaction().commit();

        final OWLClassT result = em.find(OWLClassT.class, entityT.getUri());
        assertEquals(newName, result.getName());
        assertEquals(newDescription, result.getDescription());
        assertEquals(newInt, result.getIntAttribute().intValue());
    }

    @Test
    public void updateAllowsSettingValueOfPolymorphicAttributeToInstanceOfDifferentSubtype() {
        this.em = getEntityManager("updateAllowsSettingValueOfPolymorphicAttributeToInstanceOfDifferentSubtype", true);
        persist(entityU, entityT, entityA);

        final OWLClassU newReference = new OWLClassU();
        newReference.setName("UpdatedU");
        newReference.setDescription("Description");

        em.getTransaction().begin();
        em.persist(newReference);
        final OWLClassU toUpdate = em.find(OWLClassU.class, entityU.getUri());
        toUpdate.setOwlClassS(newReference);
        em.getTransaction().commit();

        final OWLClassU result = em.find(OWLClassU.class, entityU.getUri());
        assertNotNull(result);
        assertTrue(result.getOwlClassS() instanceof OWLClassU);
        assertEquals(newReference.getUri(), result.getOwlClassS().getUri());
        assertNotNull(em.find(OWLClassS.class, entityT.getUri()));
    }
}
