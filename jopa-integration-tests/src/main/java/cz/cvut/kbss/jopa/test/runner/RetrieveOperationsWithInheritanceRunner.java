/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.Triple;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import static cz.cvut.kbss.jopa.CommonVocabulary.*;
import static org.junit.Assert.*;

public abstract class RetrieveOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public RetrieveOperationsWithInheritanceRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void findReadsAttributesOfMappedSuperclass() throws Exception {
        final Collection<Triple> data = new ArrayList<>();
        entityQ.setUri(Generators.generateUri());
        data.add(
                new Triple(entityQ.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_Q)));
        data.add(new Triple(entityQ.getUri(), URI.create(RDFS_LABEL), entityQ.getLabel()));
        data.add(
                new Triple(entityQ.getUri(), URI.create(Vocabulary.P_Q_PARENT_STRING_ATTRIBUTE),
                        entityQ.getParentString()));
        data.add(new Triple(entityQ.getUri(), URI.create(Vocabulary.P_Q_STRING_ATTRIBUTE),
                entityQ.getStringAttribute()));
        data.addAll(triplesForA());
        data.add(new Triple(entityQ.getUri(), URI.create(Vocabulary.P_HAS_OWL_CLASS_A), entityA.getUri()));
        final EntityManager em = getEntityManager("findReadsAttributesOfMappedSuperclass", false);
        persistTestData(data, em);

        final OWLClassQ result = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(result);
        assertEquals(entityQ.getStringAttribute(), result.getStringAttribute());
        assertEquals(entityQ.getLabel(), result.getLabel());
        assertEquals(entityQ.getParentString(), result.getParentString());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityQ.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    private Collection<Triple> triplesForA() {
        final Collection<Triple> data = new ArrayList<>();
        entityA.setUri(Generators.generateUri());
        data.add(
                new Triple(entityA.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_A)));
        data.add(new Triple(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                entityA.getStringAttribute()));
        return data;
    }

    @Test
    public void findReadsAttributesOfEntitySuperclass() throws Exception {
        final Collection<Triple> data = triplesForEntityT();
        final EntityManager em = getEntityManager("findReadsAttributesOfEntitySuperclass", false);
        persistTestData(data, em);

        final OWLClassT result = em.find(OWLClassT.class, entityT.getUri());
        assertNotNull(result);
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
        assertEquals(entityT.getIntAttribute(), result.getIntAttribute());
        assertNotNull(result.getOwlClassA());
        assertEquals(entityA.getUri(), result.getOwlClassA().getUri());
    }

    private Collection<Triple> triplesForEntityT() {
        final Collection<Triple> data = new ArrayList<>();
        entityT.setUri(Generators.generateUri());
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_T)));
        data.add(new Triple(entityT.getUri(), URI.create(RDFS_LABEL), entityT.getName()));
        data.add(new Triple(entityT.getUri(), URI.create(DC_DESCRIPTION), entityT.getDescription()));
        data.add(new Triple(entityT.getUri(), URI.create(Vocabulary.P_T_INTEGER_ATTRIBUTE), entityT.getIntAttribute()));
        data.addAll(triplesForA());
        data.add(new Triple(entityT.getUri(), URI.create(Vocabulary.P_HAS_OWL_CLASS_A), entityA.getUri()));
        return data;
    }

    @Test
    public void findLoadsSuperclassInstanceWhenRequestedAndClassAssertionIsPresent() throws Exception {
        final EntityManager em = getEntityManager("findLoadsSuperclassInstanceWhenRequestedAndClassAssertionIsPresent",
                false);
        final Collection<Triple> data = triplesForEntityT();
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S)));
        persistTestData(data, em);

        final OWLClassS result = em.find(OWLClassS.class, entityT.getUri());
        assertNotNull(result);
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
        assertTrue(result.getTypes().contains(Vocabulary.C_OWL_CLASS_T));
    }

    @Test
    public void findLoadsSubclassWhenSuperclassIsPassedInAndTypeCorrespondsToSubclass() throws Exception {
        final Collection<Triple> data = triplesForEntityT();

        final EntityManager em = getEntityManager(
                "findLoadsSubclassWhenSuperclassIsPassedInAndTypeCorrespondsToSubclass",
                false);
        persistTestData(data, em);

        final OWLClassS result = em.find(OWLClassS.class, entityT.getUri());
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes((OWLClassT) result);
    }

    private void verifyEntityTAttributes(OWLClassT result) {
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
        assertEquals(entityT.getIntAttribute(), result.getIntAttribute());
        assertEquals(entityT.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void findLoadsSubclassOfAbstractParent() throws Exception {
        final Collection<Triple> data = new ArrayList<>();
        entityT.setUri(Generators.generateUri());
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S)));
        data.add(new Triple(entityT.getUri(), URI.create(RDFS_LABEL), entityT.getName()));
        data.add(new Triple(entityT.getUri(), URI.create(DC_DESCRIPTION), entityT.getDescription()));

        final EntityManager em = getEntityManager("findLoadsSubclassOfAbstractParent", false);
        persistTestData(data, em);

        final OWLClassSParent result = em.find(OWLClassSParent.class, entityT.getUri());
        assertNotNull(result);
        assertTrue(result instanceof OWLClassS);
        final OWLClassS sResult = (OWLClassS) result;
        assertEquals(entityT.getName(), sResult.getName());
        assertEquals(entityT.getDescription(), sResult.getDescription());
    }

    @Test
    public void findLoadsMostConcreteSubclassOfAbstractAncestor() throws Exception {
        final Collection<Triple> data = triplesForEntityT();
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S)));

        final EntityManager em = getEntityManager("findLoadsMostConcreteSubclassOfAbstractAncestor", false);
        persistTestData(data, em);

        final OWLClassSParent result = em.find(OWLClassSParent.class, entityT.getUri());
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes((OWLClassT) result);
    }

    @Test
    public void findReturnsNullWhenMatchingClassIsAbstract() throws Exception {
        final Collection<Triple> data = triplesForEntityT();
        data.remove(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_T)));
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S_PARENT)));

        final EntityManager em = getEntityManager("findReturnsNullWhenMatchingClassIsAbstract", false);
        persistTestData(data, em);

        assertNull(em.find(OWLClassSParent.class, entityT.getUri()));
    }

    @Test
    public void findReturnsMostSpecificSubtypeWhenReturnTypeIsAbstractAncestor() throws Exception {
        final Collection<Triple> data = triplesForEntityT();
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S)));
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_S_PARENT)));

        final EntityManager em = getEntityManager("findReturnsMostSpecificSubtypeWhenReturnTypeIsAbstractAncestor",
                false);
        persistTestData(data, em);

        final OWLClassSParent result = em.find(OWLClassSParent.class, entityT.getUri());
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes((OWLClassT) result);
    }

    @Test
    public void findLoadsMostSpecificSubclassFromCache() {
        this.em = getEntityManager("findLoadsMostSpecificSubclassFromCache", true);
        persist(entityT, entityA);

        final OWLClassSParent result = em.find(OWLClassSParent.class, entityT.getUri());
        assertNotNull(result);
        assertTrue(result instanceof OWLClassT);
        verifyEntityTAttributes((OWLClassT) result);
    }

    @Test
    public void findLoadsInstanceOfSuperclassFromCacheWhenTypeMatchesAndIsSpecifiedAsReturnType() {
        this.em = getEntityManager("findLoadsInstanceOfSuperclassWhenTypeMatchesAndIsSpecifiedAsReturnType", true);
        persist(entityT, entityA);

        final OWLClassS result = em.find(OWLClassS.class, entityT.getUri());
        assertNotNull(result);
        assertEquals(entityT.getName(), result.getName());
        assertEquals(entityT.getDescription(), result.getDescription());
    }

    @Test
    public void findThrowsAmbiguousTypeExceptionWhenIndividualHasMultipleMostSpecificTypes() throws Exception {
        final Collection<Triple> data = triplesForEntityT();
        data.add(new Triple(entityT.getUri(), URI.create(RDF_TYPE), URI.create(Vocabulary.C_OWL_CLASS_U)));
        this.em = getEntityManager("findThrowsAmbiguousTypeExceptionWhenIndividualHasMultipleMostSpecificTypes", false);
        persistTestData(data, em);
        thrown.expect(AmbiguousEntityTypeException.class);

        em.find(OWLClassS.class, entityT.getUri());
    }

    @Test
    public void findThrowsAmbiguousTypeExceptionWhenIndividualHasMultipleMostSpecificTypesInCache() throws Exception {
        this.em = getEntityManager("findThrowsAmbiguousTypeExceptionWhenIndividualHasMultipleMostSpecificTypesInCache",
                true);
        persist(entityT, entityA);
        em.clear();
        final OWLClassU sameIndividual = new OWLClassU();
        sameIndividual.setUri(entityT.getUri());
        sameIndividual.setName(entityT.getName());
        persist(sameIndividual);
        assertTrue(em.getEntityManagerFactory().getCache()
                     .contains(OWLClassT.class, entityT.getUri(), new EntityDescriptor()));
        assertTrue(em.getEntityManagerFactory().getCache()
                     .contains(OWLClassU.class, sameIndividual.getUri(), new EntityDescriptor()));
        thrown.expect(AmbiguousEntityTypeException.class);

        em.find(OWLClassS.class, entityT.getUri());
    }

    @Test
    public void loadingEntityLoadsExactMatchOfPolymorphicAttribute() {
        this.em = getEntityManager("loadingEntityLoadsExactMatchOfPolymorphicAttribute", false);
        final OWLClassS s = new OWLClassS();
        s.setName("s");
        s.setDescription("S - description");
        entityU.setOwlClassS(s);
        persist(entityU, s);

        final OWLClassU result = em.find(OWLClassU.class, entityU.getUri());
        assertNotNull(result);
        assertNotNull(result.getOwlClassS());
        assertEquals(s.getUri(), result.getOwlClassS().getUri());
        assertEquals(s.getName(), result.getOwlClassS().getName());
        assertEquals(s.getDescription(), result.getOwlClassS().getDescription());
    }

    @Test
    public void loadingEntityLoadsCorrectSubtypeInPolymorphicAttribute() {
        this.em = getEntityManager("loadingEntityLoadsCorrectSubtypeInPolymorphicAttribute", false);
        persist(entityU, entityT, entityA);

        final OWLClassU result = em.find(OWLClassU.class, entityU.getUri());
        assertNotNull(result.getOwlClassS());
        assertTrue(result.getOwlClassS() instanceof OWLClassT);
        final OWLClassT tResult = (OWLClassT) result.getOwlClassS();
        verifyEntityTAttributes(tResult);
        assertNotNull(tResult.getOwlClassA());
        assertEquals(entityA.getUri(), tResult.getOwlClassA().getUri());
    }
}
