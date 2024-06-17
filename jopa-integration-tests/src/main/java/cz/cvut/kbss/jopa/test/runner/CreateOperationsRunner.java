/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exception.IdentifierNotSetException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassK;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassN;
import cz.cvut.kbss.jopa.test.OWLClassP;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr2;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr3;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr4;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr5;
import cz.cvut.kbss.jopa.test.OWLClassX;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
    void testPersistWithProperties() throws Exception {
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
        assertEquals(expected, res.getProperties());
        final List<Quad> expectedStatements = new ArrayList<>();
        props.forEach((k, vs) -> vs.forEach(v -> expectedStatements.add(new Quad(entityB.getUri(), URI.create(k), v, (String) null))));
        verifyStatementsPresent(expectedStatements, em);
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
        assertTrue(b.getProperties().isEmpty());
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
        persist(entityA);
        final EntityManager emTwo = em.getEntityManagerFactory().createEntityManager();
        try {
            persist(entityB);

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
    void testPersistTypedProperties() throws Exception {
        this.em = getEntityManager("PersistTypedProperties", false);
        entityP.setProperties(Generators.createTypedProperties());
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getProperties(), res.getProperties());
        final List<Quad> expectedStatements = new ArrayList<>();
        entityP.getProperties()
               .forEach((k, vs) -> vs.forEach(v -> expectedStatements.add(new Quad(entityP.getUri(), k, v, (String) null))));
        verifyStatementsPresent(expectedStatements, em);
    }

    @Test
    void testPersistInstanceWithPlainIdentifierObjectPropertyValue() {
        this.em = getEntityManager("PersistInstanceWithIdentifierObjectPropertyValue", false);
        final URI value = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#individualAAA");
        entityP.setIndividualUri(value);
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(value, res.getIndividualUri());
    }

    @Test
    void testPersistInstanceWithPluralObjectPropertyAttributeRepresentedByUrls() {
        this.em = getEntityManager("PersistInstanceWithPluralIdentifierObjectPropertyValue", false);
        final Set<URL> urls = Generators.createUrls();
        entityP.setIndividuals(urls);
        persist(entityP);
        em.clear();

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(urls, res.getIndividuals());
    }

    @Test
    void annotationPropertyValueIsSavedAsStringUnlessItsTypeIsUri() throws Exception {
        this.em = getEntityManager("PersistInstanceWithAnnotationPropertyValues", false);
        final String apValue = Generators.generateUri().toString();
        final URI apUriValue = Generators.generateUri();
        entityN.setAnnotationProperty(apValue);
        entityN.setAnnotationUri(apUriValue);
        persist(entityN);
        em.clear();
        assertNotNull(entityN.getId());

        final URI id = URI.create(entityN.getId());
        verifyStatementsPresent(Set.of(
                new Quad(id, URI.create(Vocabulary.P_N_STR_ANNOTATION_PROPERTY), apValue, TestEnvironment.PERSISTENCE_LANGUAGE),
                new Quad(id, URI.create(Vocabulary.P_N_URI_ANNOTATION_PROPERTY), apUriValue)
        ), em);
        final OWLClassN res = findRequired(OWLClassN.class, entityN.getId());
        assertEquals(apValue, res.getAnnotationProperty());
        assertEquals(apUriValue, res.getAnnotationUri());
    }

    @Test
    void persistEntityWithNonNullGeneratedIdentifierDoesNotRewriteIdentifier() {
        this.em = getEntityManager("PersistEntityWithNonNullGeneratedIdentifiersDoesNotRewriteIdentifier", false);
        final URI u = URI.create("http://krizik.felk.cvut.cz/ontolgoies/jopa#EntityELives");
        entityE.setUri(u);
        persist(entityE);

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
        persist(entityK, entityE);

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
        persist(entityP);
        em.clear();

        final OWLClassP result = findRequired(OWLClassP.class, entityP.getUri());
        assertEquals(entityP.getTypes().size(), result.getTypes().size());
        assertTrue(entityP.getTypes().containsAll(result.getTypes()));
    }

    @Test
    void persistEntityWithDatatypePropertyCollectionPersistsAllValues() {
        assertFalse(entityM.getIntegerSet().isEmpty());
        this.em = getEntityManager("PersistEntityWithDatatypePropertyCollection", false);
        persist(entityM);

        assertNotNull(entityM.getKey());
        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getIntegerSet(), result.getIntegerSet());
    }

    @Test
    void persistSetsStringLiteralLanguageTagAccordingToDescriptor() throws Exception {
        this.em = getEntityManager("persistSetsStringLiteralLanguageTagAccordingToDescriptor", false);
        em.getTransaction().begin();
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(em.getMetamodel().entity(OWLClassA.class)
                                          .getDeclaredAttribute("stringAttribute"), "cs");
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
        persist(entityA);

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
        descriptor.setAttributeLanguage(em.getMetamodel().entity(OWLClassA.class)
                                          .getDeclaredAttribute("stringAttribute"), null);
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
        descriptor.setAttributeLanguage(em.getMetamodel().entity(OWLClassN.class)
                                          .getDeclaredAttribute("stringAttribute"), "cs");

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
        persist(entityD);

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

        verifyStatementsPresent(entityM.getStringCollection().stream()
                                       .map(s -> new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_StringCollection), s, "en"))
                                       .collect(Collectors.toSet()), em);
    }

    @Test
    void testPersistEntityWithQueryAttr() {
        this.em = getEntityManager("PersistWithQueryAttr", false);

        em.getTransaction().begin();
        em.persist(entityWithQueryAttr);
        assertTrue(em.contains(entityWithQueryAttr));
        em.getTransaction().commit();
        em.clear();

        final OWLClassWithQueryAttr resultEntity =
                findRequired(OWLClassWithQueryAttr.class, entityWithQueryAttr.getUri());
        assertEquals(entityWithQueryAttr.getUri(), resultEntity.getUri());
        assertEquals(entityWithQueryAttr.getStringAttribute(), resultEntity.getStringAttribute());
        assertEquals(entityWithQueryAttr.getStringAttribute(), resultEntity.getStringQueryAttribute());
    }

    @Test
    void testPersistEntityWithManagedTypeQueryAttr() {
        this.em = getEntityManager("PersistWithManagedTypeQueryAttr", false);

        persist(entityA);

        em.clear();
        em.getTransaction().begin();
        em.persist(entityWithQueryAttr2);
        assertTrue(em.contains(entityWithQueryAttr2));
        em.getTransaction().commit();

        final OWLClassWithQueryAttr2 resultEntity =
                findRequired(OWLClassWithQueryAttr2.class, entityWithQueryAttr2.getUri());
        assertEquals(entityWithQueryAttr2.getUri(), resultEntity.getUri());
        assertNotNull(resultEntity.getEntityAttribute());
        assertNotNull(resultEntity.getEntityQueryAttribute());
        assertEquals(entityWithQueryAttr2.getEntityAttribute(), resultEntity.getEntityAttribute());
        assertEquals(entityWithQueryAttr2.getEntityAttribute(), resultEntity.getEntityQueryAttribute());
    }

    @Test
    void testPersistEntityWithPluralQueryAttr() {
        this.em = getEntityManager("PersistWithPluralQueryAttr", false);

        Set<String> pluralAttribute = new HashSet<>();
        pluralAttribute.add("a");
        pluralAttribute.add("b");
        pluralAttribute.add("c");

        entityWithQueryAttr3.setPluralAttribute(pluralAttribute);

        em.getTransaction().begin();
        em.persist(entityWithQueryAttr3);
        assertTrue(em.contains(entityWithQueryAttr3));
        em.getTransaction().commit();
        em.clear();

        final OWLClassWithQueryAttr3 resultEntity =
                findRequired(OWLClassWithQueryAttr3.class, entityWithQueryAttr3.getUri());
        assertEquals(entityWithQueryAttr3.getUri(), resultEntity.getUri());
        assertEquals(entityWithQueryAttr3.getPluralAttribute(), resultEntity.getPluralAttribute());
        assertEquals(entityWithQueryAttr3.getPluralAttribute(), resultEntity.getPluralQueryAttribute());
    }

    @Test
    void testPersistEntityWithPluralManagedTypeQueryAttr() {
        this.em = getEntityManager("PersistEntityWithPluralManagedTypeQueryAttr", false);

        entityWithQueryAttr5.setPluralAttribute(Collections.singleton(entityA));

        em.getTransaction().begin();
        em.persist(entityWithQueryAttr5);
        assertTrue(em.contains(entityWithQueryAttr5));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();
        em.clear();

        final OWLClassWithQueryAttr5 resultEntity =
                findRequired(OWLClassWithQueryAttr5.class, entityWithQueryAttr5.getUri());
        assertEquals(entityWithQueryAttr5.getUri(), resultEntity.getUri());
        assertEquals(entityWithQueryAttr5.getPluralAttribute(), resultEntity.getPluralAttribute());
        assertEquals(entityWithQueryAttr5.getPluralAttribute(), resultEntity.getPluralQueryAttribute());
    }

    @Test
    void testPersistEntityWithPluralMultipleManagedTypesQueryAttr() {
        this.em = getEntityManager("PersistEntityWithPluralMultipleManagedTypesQueryAttr", false);

        Set<OWLClassA> simpleSet = Generators.createSimpleSet(20);
        entityWithQueryAttr5.setPluralAttribute(simpleSet);

        em.getTransaction().begin();
        em.persist(entityWithQueryAttr5);
        assertTrue(em.contains(entityWithQueryAttr5));
        for (OWLClassA e : simpleSet) {
            assertTrue(em.contains(e));
        }
        em.getTransaction().commit();
        em.clear();

        final OWLClassWithQueryAttr5 resultEntity =
                findRequired(OWLClassWithQueryAttr5.class, entityWithQueryAttr5.getUri());
        assertEquals(entityWithQueryAttr5.getUri(), resultEntity.getUri());
        assertEquals(entityWithQueryAttr5.getPluralAttribute(), resultEntity.getPluralAttribute());
        assertEquals(entityWithQueryAttr5.getPluralAttribute(), resultEntity.getPluralQueryAttribute());
    }

    @Test
    void testPersistEntityWithASKQueryAttr() {
        this.em = getEntityManager("PersistWithASKQueryAttr", false);

        em.getTransaction().begin();
        em.persist(entityWithQueryAttr4);
        assertTrue(em.contains(entityWithQueryAttr4));
        em.getTransaction().commit();
        em.clear();

        final OWLClassWithQueryAttr4 resultEntity =
                findRequired(OWLClassWithQueryAttr4.class, entityWithQueryAttr4.getUri());
        assertEquals(entityWithQueryAttr4.getUri(), resultEntity.getUri());
        assertEquals(entityWithQueryAttr4.getStringAttribute(), resultEntity.getStringAttribute());
        assertEquals(true, resultEntity.getAskQueryAttribute());
    }

    @Test
    void persistSupportsMappingEnumsToSimpleLiterals() throws Exception {
        this.em = getEntityManager("persistSupportsMappingEnumsToSimpleLiterals", false);

        persist(entityM);

        verifyStatementsPresent(Collections.singleton(
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_enumSimpleLiteralAttribute),
                        entityM.getEnumSimpleLiteral().name(), (String) null)), em);
    }

    @Test
    void persistSupportsUsingExplicitCustomConverter() throws Exception {
        this.em = getEntityManager("persistSupportsUsingExplicitCustomConverter", false);
        final ZoneOffset value = ZoneOffset.ofHours(2);
        entityM.setWithConverter(value);

        persist(entityM);

        verifyStatementsPresent(Collections.singleton(
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_withConverter),
                        entityM.getWithConverter().getId(), (String) null)), em);
    }

    @Test
    void persistSupportsAnnotationPropertyValueMappedToSimpleLiteral() throws Exception {
        this.em = getEntityManager("persistSupportsAnnotationPropertyValueMappedToSimpleLiteral", false);
        entityM.setSimpleLiteral("test:value");
        entityM.setAnnotationSimpleLiteral("test:value");
        persist(entityM);

        verifyStatementsPresent(Arrays.asList(
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_annotationSimpleLiteral),
                        entityM.getAnnotationSimpleLiteral(), (String) null),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_simpleLiteral),
                        entityM.getSimpleLiteral(), (String) null)), em);
    }
}
