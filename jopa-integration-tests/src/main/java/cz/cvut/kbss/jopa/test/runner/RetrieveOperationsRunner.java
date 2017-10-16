/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.Triple;
import org.junit.Test;
import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public abstract class RetrieveOperationsRunner extends BaseRunner {

    public RetrieveOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRetrieveSimple() {
        this.em = getEntityManager("RetrieveSimple", false);
        persist(entityA);

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
        assertTrue(em.contains(res));
    }

    @Test(expected = NullPointerException.class)
    public void findWithNullIdentifierThrowsNPX() {
        this.em = getEntityManager("RetrieveNull", false);
        em.find(OWLClassA.class, null);
    }

    @Test
    public void testRetrieveWithLazyAttribute() throws Exception {
        this.em = getEntityManager("RetrieveLazy", false);
        persist(entityI);

        final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri());
        assertNotNull(resI);
        final Field f = OWLClassI.class.getDeclaredField("owlClassA");
        f.setAccessible(true);
        Object value = f.get(resI);
        assertNull(value);
        assertNotNull(resI.getOwlClassA());
        value = f.get(resI);
        assertNotNull(value);
        assertEquals(entityA.getUri(), resI.getOwlClassA().getUri());
        assertTrue(em.contains(resI.getOwlClassA()));
    }

    @Test
    public void testRetrieveWithGeneratedId() {
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
            final OWLClassE res = em.find(OWLClassE.class, e.getUri());
            assertNotNull(res);
            assertEquals(e.getStringAttribute(), res.getStringAttribute());
        }
    }

    @Test
    public void findByUnknownIdReturnsNull() {
        this.em = getEntityManager("RetrieveNotExisting", false);
        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNull(res);
    }

    @Test
    public void testRefreshInstance() {
        this.em = getEntityManager("Refresh", false);
        persist(entityD, entityA);

        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(d.getOwlClassA(), a);
        d.setOwlClassA(newA);
        em.refresh(d);
        assertEquals(a.getUri(), d.getOwlClassA().getUri());
    }

    @Test(expected = IllegalArgumentException.class)
    public void refreshingNotManagedIsIllegal() {
        this.em = getEntityManager("RefreshNotManaged", false);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        em.refresh(newA);
    }

    @Test
    public void findOfEntityWithExistingIdButDifferentTypeReturnsNull() {
        this.em = getEntityManager("RetrieveDifferentType", false);
        persist(entityA);

        final OWLClassB res = em.find(OWLClassB.class, entityA.getUri());
        assertNull(res);
    }

    @Test
    public void testRefreshInstanceWithUnmappedProperties() {
        this.em = getEntityManager("RefreshEntityWithProperties", false);
        final Map<URI, Set<Object>> properties = Generators.createTypedProperties();
        entityP.setProperties(properties);
        persist(entityP);

        final OWLClassP p = em.find(OWLClassP.class, entityP.getUri());
        assertNotNull(p);
        p.getProperties().put(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#addedProperty"),
                Collections.singleton("Test"));
        assertNotEquals(properties, p.getProperties());
        em.refresh(p);
        assertEquals(properties, p.getProperties());
    }

    @Test
    public void plainIdentifierAttributeIsAlwaysLoadedEagerly() throws Exception {
        this.em = getEntityManager("PlainIdentifiersAreLoadedEagerly", false);
        entityP.setIndividualUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#plainIdentifier"));
        entityP.setIndividuals(Collections.singleton(new URL("http://krizik.felk.cvut.cz/ontologies/jopa#url")));
        persist(entityP);

        final OWLClassP res = em.find(OWLClassP.class, entityP.getUri());
        final Field singularField = OWLClassP.class.getDeclaredField("individualUri");
        singularField.setAccessible(true);
        assertNotNull(singularField.get(res));
        final Field pluralField = OWLClassP.class.getDeclaredField("individuals");
        pluralField.setAccessible(true);
        assertNotNull(pluralField.get(res));
    }

    @Test
    public void readingIndividualWithStringIdTwiceInPersistenceContextReturnsSameInstance() {
        this.em = getEntityManager("readingIndividualWithStringIdTwiceInPersistenceContextReturnsSameInstance", true);
        persist(entityN);

        final OWLClassN resultOne = em.find(OWLClassN.class, entityN.getId());
        final OWLClassN resultTwo = em.find(OWLClassN.class, entityN.getId());
        assertNotNull(resultOne);
        assertNotNull(resultTwo);
        assertSame(resultOne, resultTwo);
    }

    @Test
    public void retrieveLoadsUnmappedPropertiesTogetherWithObjectPropertyValues() {
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

        final OWLClassV result = em.find(OWLClassV.class, v.getUri());
        assertNotNull(result);
        assertEquals(v.getProperties(), result.getProperties());
        final Set<String> expectedUris = v.getThings().stream().map(Thing::getUri).collect(Collectors.toSet());
        assertEquals(v.getThings().size(), result.getThings().size());
        result.getThings().forEach(t -> assertTrue(expectedUris.contains(t.getUri())));
    }

    @Test
    public void retrieveGetsStringAttributeWithCorrectLanguageWhenItIsSpecifiedInDescriptor() throws Exception {
        this.em = getEntityManager("retrieveGetsStringAttributeWithCorrectLanguageWhenItIsSpecifiedInDescriptor",
                false);
        persist(entityA);
        final String value = "v cestine";
        final String lang = "cs";
        persistTestData(Collections
                .singleton(new Triple(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value, lang)), em);

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage(lang);

        final OWLClassA result = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertEquals(value, result.getStringAttribute());
        assertEquals(entityA.getTypes(), result.getTypes());
    }

    @Test
    public void retrieveGetsStringAttributesWithDifferentLanguageTagsSpecifiedInDescriptor() throws Exception {
        this.em = getEntityManager("retrieveGetsStringAttributesWithDifferentLanguageTagsSpecifiedInDescriptor", false);
        entityN.setAnnotationProperty("english annotation");
        entityN.setStringAttribute("english string");
        persist(entityN);
        final String csAnnotation = "anotace cesky";
        final String csString = "retezec cesky";
        final Set<Triple> testData = new HashSet<>();
        testData.add(new Triple(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STR_ANNOTATION_PROPERTY),
                csAnnotation, "cs"));
        testData.add(
                new Triple(URI.create(entityN.getId()), URI.create(Vocabulary.P_N_STRING_ATTRIBUTE), csString, "cs"));
        persistTestData(testData, em);

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setAttributeLanguage(OWLClassN.class.getDeclaredField("annotationProperty"), "en");
        descriptor.setAttributeLanguage(OWLClassN.class.getDeclaredField("stringAttribute"), "cs");
        final OWLClassN result = em.find(OWLClassN.class, entityN.getId(), descriptor);
        assertEquals(entityN.getAnnotationProperty(), result.getAnnotationProperty());
        assertEquals(csString, result.getStringAttribute());
    }

    @Test
    public void retrieveAllowsToOverridePULevelLanguageSpecification() throws Exception {
        this.em = getEntityManager("retrieveAllowsToOverridePULevelLanguageSpecification", false);
        entityA.setStringAttribute(null);
        // PU-level language is en
        persist(entityA);
        final String value = "cestina";
        persistTestData(Collections
                .singleton(new Triple(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value, "cs")), em);

        final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri());
        assertNull(resOne.getStringAttribute());
        em.clear();

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("cs");
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(value, resTwo.getStringAttribute());
    }

    @Test
    public void retrieveLoadsStringLiteralWithCorrectLanguageTagWhenCachedValueHasDifferentLanguageTag()
            throws Exception {
        this.em = getEntityManager(
                "retrieveLoadsStringLiteralWithCorrectLanguageTagWhenCachedValueHasDifferentLanguageTag", true);
        persist(entityA);   // persisted with @en
        final String value = "cestina";
        persistTestData(Collections
                .singleton(new Triple(entityA.getUri(), URI.create(Vocabulary.P_A_STRING_ATTRIBUTE), value, "cs")), em);

        final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(entityA.getStringAttribute(), resOne.getStringAttribute());
        em.clear();

        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("cs");
        final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(value, resTwo.getStringAttribute());
    }
}
