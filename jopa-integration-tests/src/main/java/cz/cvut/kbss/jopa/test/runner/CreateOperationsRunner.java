/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exception.IdentifierNotSetException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

public abstract class CreateOperationsRunner extends BaseRunner {

    protected CreateOperationsRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testPersistWithGeneratedId() {
        this.em = getEntityManager("PersistWithGenerated", false);
        assertNull(entityE.getUri());
        persist(entityE);

        assertNotNull(entityE.getUri());
        final OWLClassE resE = findRequired(OWLClassE.class, entityE.getUri());
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
    }

    @Test
    void persistingEntityWithoutIdAndWithoutGeneratedIdThrowsException() {
        this.em = getEntityManager("PersistWithoutId", false);
        final OWLClassB b = new OWLClassB();
        b.setStringAttribute("someValue");
        assertThrows(IdentifierNotSetException.class, () -> persist(b));
    }

    @Test
    void persistNullThrowsNPX() {
        this.em = getEntityManager("PersistNull", false);
        em.getTransaction().begin();
        assertThrows(NullPointerException.class, () -> em.persist(null));
    }

    @Test
    void testPersistAndRollbackChanges() {
        this.em = getEntityManager("PersistRollback", false);
        em.getTransaction().begin();
        em.persist(entityE);
        assertTrue(em.contains(entityE));
        em.getTransaction().rollback();

        assertFalse(em.contains(entityE));
        assertNull(em.find(entityE.getClass(), entityE.getUri()));
    }

    @Test
    void persistingInRollbackOnlyThrowsExceptionOnCommit() {
        this.em = getEntityManager("PersistRollbackOnly", false);
        em.getTransaction().begin();
        em.getTransaction().setRollbackOnly();
        em.persist(entityE);
        assertThrows(RollbackException.class, () -> em.getTransaction().commit());
    }

    @Test
    void testPersistWithCascade() {
        this.em = getEntityManager("PersistWithCascade", false);
        persist(entityG);

        final OWLClassA resA2 = findRequired(OWLClassA.class, entityA.getUri());
        final OWLClassH resH = findRequired(OWLClassH.class, entityH.getUri());
        assertEquals(resH.getOwlClassA(), resA2);
        final OWLClassG resG = findRequired(OWLClassG.class, entityG.getUri());
        assertEquals(resG.getOwlClassH(), resH);
        assertEquals(resG.getOwlClassH().getOwlClassA(), resA2);
    }

    @Test
    void persistingOnlyOnePartOfRelationWithoutCascadeThrowsRollbackException() {
        this.em = getEntityManager("PersistWithoutCascade", false);
        assertThrows(RollbackException.class, () -> persist(entityD));
    }

    @Test
    void persistingDetachedEntityIsIllegal() {
        this.em = getEntityManager("PersistDetached", false);
        persist(entityA);

        final OWLClassA det = findRequired(OWLClassA.class, entityA.getUri());
        em.getTransaction().begin();
        em.detach(det);
        assertThrows(OWLEntityExistsException.class, () -> em.persist(det));
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

    private static void verifyLists(List<OWLClassE> expected, List<OWLClassE> actual) {
        assertEquals(expected.size(), actual.size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i).getUri(), actual.get(i).getUri());
            assertEquals(expected.get(i).getStringAttribute(), actual.get(i).getStringAttribute());
        }
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
    void testPersistWithProperties() {
        this.em = getEntityManager("PersistWithProperties", false);
        final Map<String, Set<String>> props = new HashMap<>(3);
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyOne", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/Individual10"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyTwo", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/SomeEntity"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyThree",
                Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
        final Map<String, Set<String>> expected = new HashMap<>(4);
        expected.putAll(props);
        entityB.setProperties(props);
        persist(entityB);
        em.clear();

        final OWLClassB res = findRequired(OWLClassB.class, entityB.getUri());
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertNotNull(res.getProperties());
        assertFalse(res.getProperties().isEmpty());
        assertEquals(expected.size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : expected.entrySet()) {
            assertTrue(res.getProperties().containsKey(e.getKey()));
            final Set<String> s = e.getValue();
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertNotNull(resS);
            assertEquals(1, resS.size());
            assertEquals(s.iterator().next(), resS.iterator().next());
        }
    }

    @Test
    void testPersistWithEmptyProperties() {
        this.em = getEntityManager("PersistWithPropertiesEmpty", false);
        entityB.setProperties(Collections.emptyMap());
        em.getTransaction().begin();
        em.persist(entityB);
        assertTrue(em.contains(entityB));
        em.getTransaction().commit();
        em.clear();

        final OWLClassB b = findRequired(OWLClassB.class, entityB.getUri());
        assertEquals(entityB.getUri(), b.getUri());
        assertEquals(entityB.getStringAttribute(), b.getStringAttribute());
        assertNull(b.getProperties());
    }

    @Test
    void persistTwoInstancesOfDifferentClassesWithSameUriIntoTheSamePersistenceContextIsIllegal() {
        this.em = getEntityManager("PersistURITwiceInDifferentClassesSamePC", false);
        final URI pk = URI.create("http://krizik.felk.cvut.cz/jopa/onto/sameEntity");
        final OWLClassA a = new OWLClassA();
        a.setUri(pk);
        final OWLClassB b = new OWLClassB();
        b.setUri(pk);
        em.getTransaction().begin();
        em.persist(a);
        assertThrows(OWLEntityExistsException.class, () -> em.persist(b));
    }

    @Test
    void persistTwoInstancesOfDifferentClassesWithSameUriIntoDifferentPersistenceContextsIsLegal() {
        this.em = getEntityManager("PersistURITwiceInDifferentClassesDifferentPCs", false);
        final URI uri = URI.create("http://krizik.felk.cvut.cz/jopa/onto/sameEntity");
        entityA.setUri(uri);
        entityB.setUri(uri);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        final EntityManager emTwo = em.getEntityManagerFactory().createEntityManager();
        try {
            emTwo.getTransaction().begin();
            emTwo.persist(entityB);
            emTwo.getTransaction().commit();

            assertNotNull(emTwo.find(OWLClassA.class, entityA.getUri()));
            assertNotNull(em.find(OWLClassB.class, entityB.getUri()));
        } finally {
            emTwo.close();
        }
    }

    @Test
    void testPersistEntityWithBasicTypeAttributes() {
        this.em = getEntityManager("PersistEntityWithBasicTypeAttributes", false);
        persist(entityM);
        em.clear();

        final OWLClassM res = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getKey(), res.getKey());
        assertEquals(entityM.getBooleanAttribute(), res.getBooleanAttribute());
        assertEquals(entityM.getIntAttribute(), res.getIntAttribute());
        assertEquals(entityM.getLongAttribute(), res.getLongAttribute());
        assertEquals(entityM.getFloatAttribute(), res.getFloatAttribute());
        assertEquals(entityM.getDoubleAttribute(), res.getDoubleAttribute());
        assertEquals(entityM.getDateAttribute(), res.getDateAttribute());
    }

    @Test
    void testPersistAndUpdateAttributeBeforeCommit() {
        this.em = getEntityManager("PersistAndUpdateBeforeCommit", false);
        final String updatedValue = "updatedStringAttributeValue";
        em.getTransaction().begin();
        em.persist(entityA);
        entityA.setStringAttribute(updatedValue);
        em.getTransaction().commit();
        em.clear();

        final OWLClassA res = findRequired(OWLClassA.class, entityA.getUri());
        assertEquals(updatedValue, res.getStringAttribute());
    }

    @Test
    void testPersistEntityWithEnumAttribute() {
        this.em = getEntityManager("PersistEntityWithEnum", false);
        persist(entityM);

        final OWLClassM res = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getEnumAttribute(), res.getEnumAttribute());
    }

    @Test
    void testPersistTypedProperties() {
        this.em = getEntityManager("PersistTypedProperties", false);
        entityP.setProperties(Generators.createTypedProperties());
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getProperties(), res.getProperties());
    }

    @Test
    void testPersistInstanceWithPlainIdentifierObjectPropertyValue() {
        this.em = getEntityManager("PersistInstanceWithIdentifierObjectPropertyValue", false);
        final URI value = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#individualAAA");
        entityP.setIndividualUri(value);
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(value, res.getIndividualUri());
    }

    @Test
    void testPersistInstanceWithPluralObjectPropertyAttributeRepresentedByUrls() {
        this.em = getEntityManager("PersistInstanceWithPluralIdentifierObjectPropertyValue", false);
        final Set<URL> urls = Generators.createUrls();
        entityP.setIndividuals(urls);
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(urls, res.getIndividuals());
    }

    @Test
    void testPersistInstanceWithSimpleListOfIdentifiers() {
        this.em = getEntityManager("PersistInstanceWithSimpleListOfIdentifiers", false);
        entityP.setSimpleList(Generators.createListOfIdentifiers());
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getSimpleList(), res.getSimpleList());
    }

    @Test
    void testPersistInstanceWithReferencedListOfIdentifiers() {
        this.em = getEntityManager("PersistInstanceWithReferencedListOfIdentifiers", false);
        entityP.setReferencedList(Generators.createListOfIdentifiers());
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getReferencedList(), res.getReferencedList());
    }

    @Test
    void testPersistInstanceWithAnnotationProperties() {
        this.em = getEntityManager("PersistInstanceWithAnnotationPropertyValues", false);
        final String apValue = "annotationPropertyValue";
        final URI apUriValue = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationPropertyValue");
        entityN.setAnnotationProperty(apValue);
        entityN.setAnnotationUri(apUriValue);
        em.getTransaction().begin();
        em.persist(entityN);
        em.getTransaction().commit();
        em.clear();
        assertNotNull(entityN.getId());

        final OWLClassN res = findRequired(OWLClassN.class, entityN.getId());
        assertEquals(apValue, res.getAnnotationProperty());
        assertEquals(apUriValue, res.getAnnotationUri());
    }

    @Test
    void persistEntityWithNonNullGeneratedIdentifierDoesNotRewriteIdentifier() {
        this.em = getEntityManager("PersistEntityWithNonNullGeneratedIdentifiersDoesNotRewriteIdentifier", false);
        final URI u = URI.create("http://krizik.felk.cvut.cz/ontolgoies/jopa#EntityELives");
        entityE.setUri(u);
        em.getTransaction().begin();
        em.persist(entityE);
        em.getTransaction().commit();

        assertEquals(u, entityE.getUri());
        final OWLClassE res = findRequired(OWLClassE.class, u);
        assertEquals(u, res.getUri());
        assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
    }

    @Test
    void persistEntityAndReferenceWithNonNullGeneratedIdentifiersDoesNotRewriteThem() {
        this.em = getEntityManager("PersistEntityAndReferenceWithNonNullGeneratedIdentifiersDoesNotRewriteThem", false);
        final URI uK = URI.create("http://krizik.felk.cvut.cz/ontolgoies/jopa#EntityKLives");
        final URI uE = URI.create("http://krizik.felk.cvut.cz/ontolgoies/jopa#EntityELives");
        final OWLClassK entityK = new OWLClassK();
        entityK.setUri(uK);
        entityK.setOwlClassE(entityE);
        entityE.setUri(uE);
        em.getTransaction().begin();
        em.persist(entityK);
        em.persist(entityE);
        em.getTransaction().commit();

        assertEquals(uK, entityK.getUri());
        assertEquals(uE, entityE.getUri());
        final OWLClassK resK = findRequired(OWLClassK.class, uK);
        assertEquals(uE, resK.getOwlClassE().getUri());
        final OWLClassE resE = findRequired(OWLClassE.class, uE);
        assertEquals(uE, resE.getUri());
    }

    @Test
    void testPersistEntityWithUriTypes() {
        this.em = getEntityManager("PersistEntityWithUriTypes", false);
        entityP.setTypes(Generators.createUriTypes());
        em.getTransaction().begin();
        em.persist(entityP);
        em.getTransaction().commit();
        em.clear();

        final OWLClassP result = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getTypes().size(), result.getTypes().size());
        assertTrue(entityP.getTypes().containsAll(result.getTypes()));
    }

    @Test
    void persistEntityWithDatatypePropertyCollectionPersistsAllValues() {
        assertFalse(entityM.getIntegerSet().isEmpty());
        this.em = getEntityManager("PersistEntityWithDatatypePropertyCollection", false);
        em.getTransaction().begin();
        em.persist(entityM);
        em.getTransaction().commit();

        assertNotNull(entityM.getKey());
        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getIntegerSet(), result.getIntegerSet());
    }

    @Test
    void persistSetsStringLiteralLanguageTagAccordingToDescriptor() throws Exception {
        this.em = getEntityManager("persistSetsStringLiteralLanguageTagAccordingToDescriptor", false);
        em.getTransaction().begin();
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassA.class.getDeclaredField("stringAttribute"), "cs");
        em.persist(entityA, descriptor);
        em.getTransaction().commit();

        verifyStatementsPresent(Collections.singleton(
                new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), entityA.getStringAttribute(),
                        "cs")), em);
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    void persistSetsStringLiteralLanguageTagToGloballyConfiguredValueWhenDescriptorDoesNotSpecifyIt()
            throws Exception {
        this.em = getEntityManager(
                "persistSetsStringLiteralLanguageTagToGloballyConfiguredValueWhenDescriptorDoesNotSpecifyIt", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

        verifyStatementsPresent(Collections.singleton(
                new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), entityA.getStringAttribute(),
                        "en")), em);
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    void persistAllowsOverridingGlobalLanguageWithLocalEmptyTag() throws Exception {
        this.em = getEntityManager("persistAllowsOverridingGlobalLanguageWithLocalEmptyTag", false);
        em.getTransaction().begin();
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassA.class.getDeclaredField("stringAttribute"), null);
        em.persist(entityA, descriptor);
        em.getTransaction().commit();

        verifyStatementsPresent(Collections.singleton(
                new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), entityA.getStringAttribute(),
                        (String) null)), em);
        final OWLClassA result = findRequired(OWLClassA.class, entityA.getUri());
        // The string attribute should be loaded even though PU language is set to en, because the persisted value has no lang tag
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
    }

    @Test
    void persistAllowsToSpecifyLanguageTagPerEntityAndOverrideItOnAttributeLevel() throws Exception {
        this.em = getEntityManager("persistAllowsToSpecifyLanguageTagPerEntityAndOverrideItOnAttributeLevel", false);
        entityN.setStringAttribute("retezec v cestine");
        entityN.setAnnotationProperty("entity descriptor ist in Deutsch");
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("de");
        descriptor.setAttributeLanguage(OWLClassN.class.getDeclaredField("stringAttribute"), "cs");

        em.getTransaction().begin();
        em.persist(entityN, descriptor);
        em.getTransaction().commit();

        final Set<Quad> statements = new HashSet<>(4);
        statements.add(new Quad(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STR_ANNOTATION_PROPERTY),
                entityN.getAnnotationProperty(), "de"));
        statements.add(new Quad(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STRING_ATTRIBUTE),
                entityN.getStringAttribute(), "cs"));
        verifyStatementsPresent(statements, em);

        final OWLClassN result = em.find(OWLClassN.class, entityN.getId(), descriptor);
        assertEquals(entityN.getAnnotationProperty(), result.getAnnotationProperty());
        assertEquals(entityN.getStringAttribute(), result.getStringAttribute());
    }

    @Test
    void persistSavesInstanceWithReferenceToExistingInstance() {
        this.em = getEntityManager("persistSavesInstanceWithReferenceToExistingInstance", false);
        persist(entityA);

        em.clear();
        em.getTransaction().begin();
        em.persist(entityD);
        em.getTransaction().commit();

        final OWLClassD resultD = findRequired(OWLClassD.class, entityD.getUri());
        assertNotNull(resultD.getOwlClassA());
    }

    @Test
    void persistSupportsLocalDateTimeApi() {
        this.em = getEntityManager("persistSupportsLocalDateTimeApi", false);
        final OWLClassX entityX = new OWLClassX();
        final LocalDate date = LocalDate.now();
        entityX.setLocalDate(date);
        // Truncate to millis to prevent problems with storage precision
        final LocalDateTime dateTime = LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS);
        entityX.setLocalDateTime(dateTime);
        persist(entityX);

        final OWLClassX result = findRequired(OWLClassX.class, entityX.getUri());
        assertEquals(date, result.getLocalDate());
        assertEquals(dateTime, result.getLocalDateTime());
    }

    @Test
    void persistSupportsPluralAnnotationProperties() {
        this.em = getEntityManager("persistSupportsPluralAnnotationProperties", false);
        final Set<String> annotations = IntStream.range(0, 5).mapToObj(i -> "Source" + i).collect(Collectors.toSet());
        entityN.setPluralAnnotationProperty(annotations);
        persist(entityN);

        final OWLClassN result = findRequired(OWLClassN.class, entityN.getId());
        assertEquals(annotations, result.getPluralAnnotationProperty());
    }

    @Test
    void persistSupportsSavingSimpleLiteralValue() {
        this.em = getEntityManager("persistSupportsSavingSimpleLiteralValue", false);
        final String value = "test";
        entityM.setSimpleLiteral(value);
        persist(entityM);

        verifyValueDatatype(URI.create(entityM.getKey()), Vocabulary.p_m_simpleLiteral, XSD.STRING);
        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(value, result.getSimpleLiteral());
    }

    @Test
    void persistSupportsDataCollectionAttribute() throws Exception {
        this.em = getEntityManager("persistSupportsDataCollectionAttribute", false);
        assertFalse(entityM.getStringCollection().isEmpty());
        persist(entityM);

        verifyStatementsPresent(entityM.getStringCollection().stream().map(s -> new Quad(URI.create(entityM.getKey()),
                URI.create(Vocabulary.p_m_StringCollection), s, "en")).collect(
                Collectors.toSet()), em);
    }

    @Test
    void persistSupportsMultilingualAttributes() throws Exception {
        final Map<String, String> translations = new HashMap<>();
        translations.put("en", "building");
        translations.put("cs", "stavba");
        translations.put("de", "der Bau");
        this.em = getEntityManager("persistSupportsMultilingualAttributes", false);

        final OWLClassY toPersist = new OWLClassY();
        toPersist.setSingularString(new MultilingualString(translations));
        persist(toPersist);

        final OWLClassY result = findRequired(OWLClassY.class, toPersist.getUri());
        assertNotNull(result.getSingularString());
        assertEquals(translations, result.getSingularString().getValue());
        verifyStatementsPresent(translations.entrySet().stream().map(e -> new Quad(toPersist.getUri(),
                URI.create(Vocabulary.P_Y_SINGULAR_MULTILINGUAL_ATTRIBUTE), e.getValue(), e.getKey())).collect(
                Collectors.toSet()), em);
    }
}
