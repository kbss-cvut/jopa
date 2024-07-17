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

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassN;
import cz.cvut.kbss.jopa.test.OWLClassP;
import cz.cvut.kbss.jopa.test.OWLClassV;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr2;
import cz.cvut.kbss.jopa.test.OWLClassWithQueryAttr6;
import cz.cvut.kbss.jopa.test.OWLClassWithUrn;
import cz.cvut.kbss.jopa.test.Thing;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.ReloadableDataSource;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;


public abstract class RetrieveOperationsRunner extends BaseRunner {

    public RetrieveOperationsRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void testRetrieveSimple() {
        this.em = getEntityManager("RetrieveSimple", false);
        persist(entityA);

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassA res = findRequired(OWLClassA.class, entityA.getUri());
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
        assertTrue(em.contains(res));
    }

    @Test
    void testRetrieveWithLazyAttribute() {
        this.em = getEntityManager("RetrieveLazy", false);
        persist(entityI);

        final OWLClassI resI = findRequired(OWLClassI.class, entityI.getUri());
        assertInstanceOf(LazyLoadingProxy.class, resI.getOwlClassA());
        // Trigger lazy loading
        assertNotNull(resI.getOwlClassA().getUri());
        assertEquals(entityA.getUri(), resI.getOwlClassA().getUri());
        assertTrue(em.contains(resI.getOwlClassA()));
    }

    @Test
    void testRetrieveWithGeneratedId() {
        this.em = getEntityManager("RetrieveGenerated", false);
        em.getTransaction().begin();
        final int size = 10;
        final List<OWLClassE> lst = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("blablabla" + i);
            assertNull(e.getUri());
            em.persist(e);
            assertNotNull(e.getUri());
            lst.add(e);
        }
        em.getTransaction().commit();

        em.clear();
        for (OWLClassE e : lst) {
            final OWLClassE res = findRequired(OWLClassE.class, e.getUri());
            assertEquals(e.getStringAttribute(), res.getStringAttribute());
        }
    }

    @Test
    void findByUnknownIdReturnsNull() {
        this.em = getEntityManager("RetrieveNotExisting", false);
        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNull(res);
    }

    @Test
    void testRefreshInstance() {
        this.em = getEntityManager("Refresh", false);
        persist(entityD, entityA);

        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        final OWLClassD d = findRequired(OWLClassD.class, entityD.getUri());
        final OWLClassA a = findRequired(OWLClassA.class, entityA.getUri());
        assertEquals(d.getOwlClassA(), a);
        d.setOwlClassA(newA);
        em.refresh(d);
        assertEquals(a.getUri(), d.getOwlClassA().getUri());
    }

    @Test
    void refreshingNotManagedIsIllegal() {
        this.em = getEntityManager("RefreshNotManaged", false);
        persist(entityA);

        findRequired(OWLClassA.class, entityA.getUri());
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        assertThrows(IllegalArgumentException.class, () -> em.refresh(newA));
    }

    @Test
    void findOfEntityWithExistingIdButDifferentTypeReturnsNull() {
        this.em = getEntityManager("RetrieveDifferentType", false);
        persist(entityA);

        final OWLClassB res = em.find(OWLClassB.class, entityA.getUri());
        assertNull(res);
    }

    @Test
    void testRefreshInstanceWithUnmappedProperties() {
        this.em = getEntityManager("RefreshEntityWithProperties", false);
        final Map<URI, Set<Object>> properties = Generators.createTypedProperties();
        entityP.setProperties(properties);
        persist(entityP);

        final OWLClassP p = findRequired(OWLClassP.class, entityP.getUri());
        p.getProperties().put(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#addedProperty"),
                Collections.singleton("Test"));
        assertNotEquals(properties, p.getProperties());
        em.refresh(p);
        assertEquals(properties, p.getProperties());
    }

    @Test
    void plainIdentifierAttributeIsAlwaysLoadedEagerly() throws Exception {
        this.em = getEntityManager("PlainIdentifiersAreLoadedEagerly", false);
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#plainIdentifier"));
        entityP.setIndividuals(Collections.singleton(new URL("http://krizik.felk.cvut.cz/ontologies/jopa#url")));
        persist(entityP);

        final OWLClassP res = findRequired(OWLClassP.class, entityP.getUri());
        final Field singularField = OWLClassP.class.getDeclaredField("individualUri");
        singularField.setAccessible(true);
        assertNotNull(singularField.get(res));
        final Field pluralField = OWLClassP.class.getDeclaredField("individuals");
        pluralField.setAccessible(true);
        assertNotNull(pluralField.get(res));
    }

    @Test
    void readingIndividualWithStringIdTwiceInPersistenceContextReturnsSameInstance() {
        this.em = getEntityManager("readingIndividualWithStringIdTwiceInPersistenceContextReturnsSameInstance", true);
        persist(entityN);

        final OWLClassN resultOne = findRequired(OWLClassN.class, entityN.getId());
        final OWLClassN resultTwo = findRequired(OWLClassN.class, entityN.getId());
        assertSame(resultOne, resultTwo);
    }

    @Test
    void retrieveLoadsUnmappedPropertiesTogetherWithObjectPropertyValues() {
        this.em = getEntityManager("retrieveLoadsUnmappedPropertiesTogetherWithObjectPropertyValues", false);
        final OWLClassV v = new OWLClassV();
        v.setProperties(Generators.createProperties());
        v.setThings(new HashSet<>());
        for (int i = 0; i < Generators.randomPositiveInt(5, 10); i++) {
            final Thing thing = new Thing();
            thing.setName("thing" + i);
            thing.setDescription("description of a thing. Number " + i);
            thing.setTypes(Collections.singleton(Vocabulary.C_OWL_CLASS_A));
            v.getThings().add(thing);
        }
        em.getTransaction().begin();
        em.persist(v);
        v.getThings().forEach(em::persist);
        em.getTransaction().commit();
        em.clear();

        final OWLClassV result = findRequired(OWLClassV.class, v.getUri());
        assertEquals(v.getProperties(), result.getProperties());
        final Set<String> expectedUris = v.getThings().stream().map(Thing::getUri).collect(Collectors.toSet());
        assertEquals(v.getThings().size(), result.getThings().size());
        result.getThings().forEach(t -> assertTrue(expectedUris.contains(t.getUri())));
    }

    @Test
    void retrieveGetsStringAttributeWithCorrectLanguageWhenItIsSpecifiedInDescriptor() throws Exception {
        this.em = getEntityManager("retrieveGetsStringAttributeWithCorrectLanguageWhenItIsSpecifiedInDescriptor",
                false);
        persist(entityA);
        final String value = "v cestine";
        final String lang = "cs";
        persistTestData(Collections
                .singleton(
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value,
                                lang)), em);

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage(lang);

        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertEquals(value, result.getStringAttribute());
        assertEquals(entityA.getTypes(), result.getTypes());
    }

    @Test
    void retrieveGetsStringAttributesWithDifferentLanguageTagsSpecifiedInDescriptor() throws Exception {
        this.em = getEntityManager("retrieveGetsStringAttributesWithDifferentLanguageTagsSpecifiedInDescriptor", false);
        entityN.setAnnotationProperty("english annotation");
        entityN.setStringAttribute("english string");
        persist(entityN);
        final String csAnnotation = "anotace cesky";
        final String csString = "retezec cesky";
        final Set<Quad> testData = new HashSet<>();
        testData.add(new Quad(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STR_ANNOTATION_PROPERTY),
                csAnnotation, "cs"));
        testData.add(
                new Quad(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STRING_ATTRIBUTE), csString, "cs"));
        persistTestData(testData, em);

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(
                em.getMetamodel().entity(OWLClassN.class).getDeclaredAttribute("annotationProperty"), "en");
        descriptor
                .setAttributeLanguage(em.getMetamodel().entity(OWLClassN.class).getDeclaredAttribute("stringAttribute"),
                        "cs");
        final OWLClassN result = em.find(OWLClassN.class, entityN.getId(), descriptor);
        assertEquals(entityN.getAnnotationProperty(), result.getAnnotationProperty());
        assertEquals(csString, result.getStringAttribute());
    }

    @Test
    void retrieveAllowsToOverridePULevelLanguageSpecification() throws Exception {
        this.em = getEntityManager("retrieveAllowsToOverridePULevelLanguageSpecification", false);
        entityA.setStringAttribute(null);
        // PU-level language is en
        persist(entityA);
        final String value = "cestina";
        persistTestData(Collections
                .singleton(
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value,
                                "cs")), em);

        final OWLClassA resOne = findRequired(OWLClassA.class, entityA.getUri());
        assertNull(resOne.getStringAttribute());
        em.clear();

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("cs");
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(value, resTwo.getStringAttribute());
    }

    @Test
    void retrieveLoadsStringLiteralWithCorrectLanguageTagWhenCachedValueHasDifferentLanguageTag()
            throws Exception {
        this.em = getEntityManager(
                "retrieveLoadsStringLiteralWithCorrectLanguageTagWhenCachedValueHasDifferentLanguageTag", true);
        persist(entityA);   // persisted with @en
        final String value = "cestina";
        persistTestData(Collections
                .singleton(
                        new Quad(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value,
                                "cs")), em);

        final OWLClassA resOne = findRequired(OWLClassA.class, entityA.getUri());
        assertEquals(entityA.getStringAttribute(), resOne.getStringAttribute());
        em.clear();

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("cs");
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(value, resTwo.getStringAttribute());
    }

    @Test
    public void reloadAllowsToReloadFileStorageContent() throws Exception {
        final Map<String, String> props = new HashMap<>();
        final File storage = Files.createTempFile("reload-driver-test", ".owl").toFile();
        storage.deleteOnExit();
        final String initialContent = """
                <?xml version="1.0"?>
                <rdf:RDF
                  xmlns:owl = "http://www.w3.org/2002/07/owl#"
                  xmlns:rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\
                <owl:Ontology rdf:about="http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine"></owl:Ontology>\
                </rdf:RDF>""";
        Files.write(storage.toPath(), initialContent.getBytes());
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, storage.toURI().toString());
        props.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine");
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.toString(false));
        addFileStorageProperties(props);
        this.em = getEntityManager("reloadAllowsToReloadFileStorageContent", false, props);
        final String subject = "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CaliforniaRegion";
        final String type = "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Region";
        em.getTransaction().begin();
        final TypedQuery<Boolean> query =
                em.createNativeQuery("ASK { ?x a ?y . }", Boolean.class).setParameter("x", URI.create(subject))
                  .setParameter("y", URI.create(type));
        assertFalse(query.getSingleResult());
        replaceFileContents(storage);

        final ReloadableDataSource ds = em.getEntityManagerFactory().unwrap(ReloadableDataSource.class);
        ds.reload();
        // Need to force OWLAPI driver to open a new ontology snapshot with the reloaded data
        em.getTransaction().commit();
        assertTrue(query.getSingleResult());
    }

    private static void replaceFileContents(File target) throws IOException {
        try (final InputStream is = new URL("https://www.w3.org/TR/2003/PR-owl-guide-20031209/wine").openStream()) {
            Files.copy(is, target.toPath(), StandardCopyOption.REPLACE_EXISTING);
        }
    }

    protected abstract void addFileStorageProperties(Map<String, String> properties);

    @Test
    void getReferenceRetrievesReferenceToInstanceWithDataPropertiesWhoseAttributesAreLoadedLazily() throws Exception {
        this.em = getEntityManager(
                "getReferenceRetrievesReferenceToInstanceWithDataPropertiesWhoseAttributesAreLoadedLazily", false);
        persist(entityM);
        final OWLClassM result = em.getReference(OWLClassM.class, entityM.getKey());
        assertNotNull(result);
        assertTrue(em.contains(result));
        final Field intAttField = OWLClassM.class.getDeclaredField("intAttribute");
        intAttField.setAccessible(true);
        assertNull(intAttField.get(result));
        assertNotNull(result.getIntAttribute());
        assertEquals(entityM.getBooleanAttribute(), result.getBooleanAttribute());
        assertEquals(entityM.getIntAttribute(), result.getIntAttribute());
        assertEquals(entityM.getIntegerSet(), result.getIntegerSet());
        assertEquals(entityM.getDateAttribute(), result.getDateAttribute());
        assertEquals(entityM.getEnumAttribute(), result.getEnumAttribute());
    }

    @Test
    void getReferenceRetrievesReferenceToInstanceWithObjectPropertiesWhoseAttributesAreLoadedLazily() {
        this.em = getEntityManager(
                "getReferenceRetrievesReferenceToInstanceWithObjectPropertiesWhoseAttributesAreLoadedLazily", false);
        persist(entityG);
        final OWLClassG gResult = em.getReference(OWLClassG.class, entityG.getUri());
        assertNotNull(gResult);
        assertNotNull(gResult.getOwlClassH());
        assertEquals(entityH.getUri(), gResult.getOwlClassH().getUri());
        assertNotNull(gResult.getOwlClassH().getOwlClassA());
        assertEquals(entityA.getUri(), gResult.getOwlClassH().getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), gResult.getOwlClassH().getOwlClassA().getStringAttribute());
    }

    @Test
    void getReferenceRetrievesReferenceToInstanceWhenCacheIsEnabled() {
        this.em = getEntityManager("getReferenceRetrievesReferenceToInstanceWhenCacheIsEnabled", true);
        persist(entityA);

        final OWLClassA result = em.getReference(OWLClassA.class, entityA.getUri());
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
    }

    @Test
    void loadingEntityWithLexicalFormAttributeLoadsLexicalFormOfLiteral() throws Exception {
        this.em = getEntityManager("loadingEntityWithLexicalFormAttributeLoadsLexicalFormOfLiteral", false);
        persist(entityM);
        final Integer value = 117;
        persistTestData(Collections
                        .singleton(
                                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_lexicalForm),
                                        value)),
                em);
        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(value.toString(), result.getLexicalForm());
    }

    @Test
    void loadingEntityWithSimpleLiteralLoadsSimpleLiteralValue() throws Exception {
        this.em = getEntityManager("loadingEntityWithSimpleLiteralLoadsSimpleLiteralValue", false);
        final String value = "test";
        persistTestData(Arrays.asList(
                        new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                        new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_simpleLiteral), value, (String) null)),
                em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(value, result.getSimpleLiteral());
    }

    @Test
    void loadingEntityWithSimpleLiteralUriLoadsSimpleLiteralValue() throws Exception {
        this.em = getEntityManager("loadingEntityWithSimpleLiteralUriLoadsSimpleLiteralValue", false);
        final URI value = Generators.generateUri();
        persistTestData(Arrays.asList(
                        new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                        new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_simpleLiteralUri), value, (String) null)),
                em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(value, result.getSimpleLiteralUri());
    }

    @Test
    void loadEntityWithSimpleLiteralLoadsAlsoLanguageTaggedValue() throws Exception {
        this.em = getEntityManager("loadEntityWithSimpleLiteralLoadsAlsoLanguageTaggedValue", false);
        final String value = "test";
        persistTestData(Arrays.asList(
                new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_simpleLiteral), value, "en")), em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(value, result.getSimpleLiteral());
    }

    @Test
    void loadEntitySupportsCollectionAttribute() throws Exception {
        this.em = getEntityManager("loadEntitySupportsCollectionAttribute", false);
        persistTestData(Arrays.asList(
                        new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                        new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_StringCollection), "value", "en"),
                        new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_StringCollection), "valueTwo", "en")),
                em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertNotNull(result.getStringCollection());
        assertFalse(result.getStringCollection().isEmpty());
        assertThat(result.getStringCollection(), hasItems("value", "valueTwo"));
    }

    @Test
    void testRetrieveEntityWithQueryAttr() {
        this.em = getEntityManager("RetrieveWithQueryAttr", false);

        persist(entityWithQueryAttr);

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassWithQueryAttr res = findRequired(OWLClassWithQueryAttr.class, entityWithQueryAttr.getUri());
        assertEquals(entityWithQueryAttr.getUri(), res.getUri());
        assertEquals(entityWithQueryAttr.getStringAttribute(), res.getStringAttribute());
        assertEquals(entityWithQueryAttr.getStringAttribute(), res.getStringQueryAttribute());
        assertTrue(em.contains(res));
    }

    @Test
    void testRetrieveEntityWithManagedTypeQueryAttr() {
        this.em = getEntityManager("RetrieveWithManagedTypeQueryAttr", false);

        persist(entityWithQueryAttr2, entityA);

        final OWLClassWithQueryAttr2 res = findRequired(OWLClassWithQueryAttr2.class, entityWithQueryAttr2.getUri());
        assertEquals(entityWithQueryAttr2.getUri(), res.getUri());
        assertEquals(entityWithQueryAttr2.getEntityAttribute(), res.getEntityAttribute());
        assertEquals(entityWithQueryAttr2.getEntityAttribute(), res.getEntityQueryAttribute());
        assertTrue(em.contains(res));
    }

    @Test
    void testRetrieveWithLazyQueryAttribute() throws Exception {
        this.em = getEntityManager("RetrieveLazyQueryAttr", false);

        entityWithQueryAttr6.setOwlClassA(entityA);
        entityD.setOwlClassA(entityA);


        persist(entityWithQueryAttr6, entityD);

        final OWLClassWithQueryAttr6 res = findRequired(OWLClassWithQueryAttr6.class, entityWithQueryAttr6.getUri());
        final Field f = OWLClassWithQueryAttr6.class.getDeclaredField("lazyQueryAttribute");
        f.setAccessible(true);
        Object value = f.get(res);
        assertInstanceOf(LazyLoadingProxy.class, value);
        assertNotNull(res.getLazyQueryAttribute());
        value = f.get(res);
        assertNotNull(value);
        assertEquals(entityD.getUri(), res.getLazyQueryAttribute().getUri());
    }

    @Test
    void testSupportForUrnIrisInClassAndProperty() {
        this.em = getEntityManager("testSupportForUrnIrisInClassAndProperty", false);
        final OWLClassWithUrn instance = new OWLClassWithUrn();
        instance.setUri(Generators.generateUri());
        instance.setLabel("Test instance with URN");
        instance.setCreated(LocalDate.now());
        persist(instance);

        final OWLClassWithUrn result = findRequired(OWLClassWithUrn.class, instance.getUri());
        assertEquals(instance.getLabel(), result.getLabel());
        assertEquals(instance.getCreated(), result.getCreated());
    }

    @Test
    void testSupportForUrnIdentifier() {
        this.em = getEntityManager("testSupportForUrnIdentifier", false);
        final URI id = URI.create("urn:jopa:instance:test-instance");
        final OWLClassWithUrn instance = new OWLClassWithUrn();
        instance.setUri(id);
        persist(instance);

        findRequired(OWLClassWithUrn.class, id);
    }

    @Test
    void retrieveSupportsMappingEnumsToSimpleLiterals() throws Exception {
        this.em = getEntityManager("retrieveSupportsMappingEnumsToSimpleLiterals", false);
        persistTestData(List.of(
                new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_enumSimpleLiteralAttribute),
                        entityM.getEnumSimpleLiteral().name(), (String) null)
        ), em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(entityM.getEnumSimpleLiteral(), result.getEnumSimpleLiteral());
    }

    @Test
    void retrieveViaReflectiveGetter() throws Exception {
        this.em = getEntityManager("retrieveViaReflectiveGetter", false);
        transactional(() -> {
            em.persist(entityC);
            entityC.setReferencedList(List.of(entityA));
            entityC.getReferencedList().forEach(em::persist);
        });
        em.clear();

        final OWLClassC result = em.find(OWLClassC.class, entityC.getUri());
        final Method getter = OWLClassC.class.getDeclaredMethod("getReferencedList");
        final Object value = getter.invoke(result);
        assertNotNull(value);
        assertFalse(((List) value).isEmpty());

        assertEquals(1, ((List) value).size());
        assertThat(result.getReferencedList(), not(instanceOf(LazyLoadingProxy.class)));
    }

    @Test
    void lazilyLoadedAttributeIsNullWhenThereIsNoReferenceToLoad() {
        this.em = getEntityManager("lazilyLoadedAttributeIsNullWhenThereIsNoReferenceToLoad", false);
        final EntityDescriptor descriptor = new EntityDescriptor(Generators.generateUri());
        entityI.setOwlClassA(null);
        transactional(() -> em.persist(entityI, descriptor));

        final OWLClassI result = findRequired(OWLClassI.class, entityI.getUri());
        assertNull(result.getOwlClassA());
    }
}
