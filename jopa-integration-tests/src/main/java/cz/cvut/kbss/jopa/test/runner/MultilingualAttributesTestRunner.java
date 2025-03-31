/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassY;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.junit.jupiter.api.Assertions.*;

public abstract class MultilingualAttributesTestRunner extends BaseRunner {

    protected MultilingualAttributesTestRunner(Logger logger, PersistenceFactory persistenceFactory,
                                               DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void loadEntitySupportsSingularMultilingualAttribute() throws Exception {
        this.em = getEntityManager("loadEntitySupportsSingularMultilingualAttribute", false);
        final URI uri = Generators.generateUri();
        final Map<String, String> translations = generateMultilingualData(uri);

        final OWLClassY result = findRequired(OWLClassY.class, uri);
        assertNotNull(result.getSingularString());
        translations.forEach((key, value) -> {
            assertTrue(result.getSingularString().contains(key));
            assertEquals(value, result.getSingularString().get(key));
        });
    }

    private Map<String, String> generateMultilingualData(URI uri) throws Exception {
        final URI singularProperty = URI.create(Vocabulary.P_Y_SINGULAR_MULTILINGUAL_ATTRIBUTE);
        final Map<String, String> translations = new HashMap<>();
        translations.put("en", "building");
        translations.put("cs", "stavba");
        translations.put("de", "der Bau");
        final Collection<Quad> quads = new ArrayList<>();
        quads.add(new Quad(uri, URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_Y)));
        translations.entrySet().stream().map(e -> new Quad(uri, singularProperty, e.getValue(), e.getKey())).forEach(
                quads::add);
        persistTestData(quads, em);
        return translations;
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

    @Test
    void removeDeletesAllValuesOfMultilingualAttribute() throws Exception {
        this.em = getEntityManager("removeDeletesAllValuesOfMultilingualAttribute", false);
        final URI uri = Generators.generateUri();
        generateMultilingualData(uri);

        em.getTransaction().begin();
        final OWLClassY toRemove = findRequired(OWLClassY.class, uri);
        assertNotNull(toRemove.getSingularString());
        assertFalse(toRemove.getSingularString().isEmpty());
        em.remove(toRemove);
        em.getTransaction().commit();

        assertFalse(em.createNativeQuery("ASK { ?x ?singularString ?y . }", Boolean.class)
                .setParameter("x", toRemove.getUri())
                .setParameter("singularString", URI.create(Vocabulary.P_Y_SINGULAR_MULTILINGUAL_ATTRIBUTE))
                .getSingleResult());
    }

    @Test
    void settingMultilingualStringAttributeToNullRemovesAllValues() {
        this.em = getEntityManager("settingMultilingualStringAttributeToNullRemovesAllValues", false);
        final OWLClassY entityY = new OWLClassY();
        entityY.setSingularString(new MultilingualString());
        entityY.getSingularString().set("en", "building");
        entityY.getSingularString().set("cs", "stavba");
        entityY.getSingularString().set("de", "der Bau");
        persist(entityY);

        em.getTransaction().begin();
        final OWLClassY toRemove = findRequired(OWLClassY.class, entityY.getUri());
        assertNotNull(toRemove.getSingularString());
        toRemove.setSingularString(null);
        em.getTransaction().commit();

        final OWLClassY result = findRequired(OWLClassY.class, entityY.getUri());
        assertNull(result.getSingularString());
    }

    @Test
    void updateSupportsAddingTranslationsToMultilingualAttribute() {
        this.em = getEntityManager("updateSupportsAddingTranslationsToMultilingualAttribute", true);
        final OWLClassY y = new OWLClassY();
        y.setSingularString(new MultilingualString());
        y.getSingularString().set("en", "building");
        persist(y);

        y.getSingularString().set("cs", "stavba");
        y.getSingularString().set("de", "der Bau");
        transactional(() -> em.merge(y));

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertEquals(3, result.getSingularString().getLanguages().size());
        assertEquals(y.getSingularString(), result.getSingularString());
    }

    @Test
    void updateSupportsReplacingTranslationsInMultilingualAttribute() {
        this.em = getEntityManager("updateSupportsReplacingTranslationsInMultilingualAttribute", true);
        final OWLClassY y = new OWLClassY();
        y.setSingularString(new MultilingualString());
        y.getSingularString().set("en", "construction");
        persist(y);

        final String replacement = "building";
        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.getSingularString().set("en", replacement);
            toUpdate.getSingularString().set("cs", "stavba");
        });

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertEquals(2, result.getSingularString().getLanguages().size());
        assertEquals(replacement, result.getSingularString().get("en"));
        assertEquals("stavba", result.getSingularString().get("cs"));
    }

    @Test
    void updateSupportsCompletelyReplacingMultilingualAttributeValueInTransaction() {
        this.em = getEntityManager("updateSupportsCompletelyReplacingMultilingualAttributeValueInTransaction", true);
        final OWLClassY y = new OWLClassY();
        y.setSingularString(new MultilingualString());
        y.getSingularString().set("en", "construction");
        persist(y);

        final String replacement = "building";
        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.setSingularString(new MultilingualString());
            toUpdate.getSingularString().set("en", replacement);
            toUpdate.getSingularString().set("cs", "stavba");
        });

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertEquals(2, result.getSingularString().getLanguages().size());
        assertEquals(replacement, result.getSingularString().get("en"));
        assertEquals("stavba", result.getSingularString().get("cs"));
    }

    @Test
    void persistSupportsPluralMultilingualAttributes() {
        this.em = getEntityManager("persistSupportsPluralMultilingualAttributes", false);
        final OWLClassY y = persistYWithPluralMultilingualAttributeValues();

        final URI property = URI.create(Vocabulary.P_Y_PLURAL_MULTILINGUAL_ATTRIBUTE);
        transactionalThrowing(
                () -> verifyStatementsPresent(Arrays.asList(new Quad(y.getUri(), property, "construction", "en"),
                        new Quad(y.getUri(), property, "stavba", "cs"),
                        new Quad(y.getUri(), property, "building", "en"),
                        new Quad(y.getUri(), property, "budova", "cs")), em));
    }

    @Test
    void loadEntitySupportsPluralMultilingualAttributes() throws Exception {
        this.em = getEntityManager("loadEntitySupportsPluralMultilingualAttributes", true);
        final URI individual = Generators.generateUri();
        final URI singularProperty = URI.create(Vocabulary.P_Y_PLURAL_MULTILINGUAL_ATTRIBUTE);
        final Map<String, Set<String>> translations = new HashMap<>();
        translations.put("en", new HashSet<>(Arrays.asList("building", "construction")));
        translations.put("cs", new HashSet<>(Arrays.asList("stavba", "budova")));
        translations.put("de", Collections.singleton("der Bau"));
        final Collection<Quad> quads = new ArrayList<>();
        quads.add(new Quad(individual, URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_Y)));
        translations.entrySet().stream()
                .flatMap(e -> e.getValue().stream().map(s -> new Quad(individual, singularProperty, s, e.getKey())))
                .forEach(quads::add);
        persistTestData(quads, em);

        final OWLClassY result = findRequired(OWLClassY.class, individual);
        assertNotNull(result.getPluralString());
        for (Map.Entry<String, Set<String>> e : translations.entrySet()) {
            for (String s : e.getValue()) {
                verifyPluralContainsValue(result, e.getKey(), s);
            }
        }
    }

    private static void verifyPluralContainsValue(OWLClassY instance, String lang, String value) {
        assertTrue(instance.getPluralString().stream()
                .anyMatch(ms -> ms.contains(lang) && ms.get(lang).equals(value)));
    }

    @Test
    void updateSupportsReplacingValuesInPluralMultilingualAttributes() {
        this.em = getEntityManager("updateSupportsReplacingValuesInPluralMultilingualAttributes", true);
        final OWLClassY y = persistYWithPluralMultilingualAttributeValues();

        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.getPluralString().iterator().next().set("de", "der Bau");
        });
        em.getEntityManagerFactory().getCache().evictAll();

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        verifyPluralContainsValue(result, "de", "der Bau");
        verifyPluralContainsValue(result, "en", "construction");
        verifyPluralContainsValue(result, "cs", "stavba");
        verifyPluralContainsValue(result, "en", "building");
        verifyPluralContainsValue(result, "cs", "budova");
    }

    private OWLClassY persistYWithPluralMultilingualAttributeValues() {
        final OWLClassY y = new OWLClassY();
        final MultilingualString sOne = new MultilingualString();
        sOne.set("en", "construction");
        sOne.set("cs", "stavba");
        final MultilingualString sTwo = new MultilingualString();
        sTwo.set("en", "building");
        sTwo.set("cs", "budova");
        y.setPluralString(new HashSet<>(Arrays.asList(sOne, sTwo)));
        persist(y);
        return y;
    }

    @Test
    void updateSupportsRemovingValuesFromPluralMultilingualAttributes() {
        this.em = getEntityManager("updateSupportsRemovingValuesFromPluralMultilingualAttributes", true);
        final OWLClassY y = persistYWithPluralMultilingualAttributeValues();

        transactional(() -> {
            final Iterator<MultilingualString> it = y.getPluralString().iterator();
            it.next();
            it.remove();
            em.merge(y);
        });

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertEquals(1, result.getPluralString().size());
    }

    @Test
    void clearingPluralMultilingualAttributeValueRemovesAllItsValues() {
        this.em = getEntityManager("settingPluralMultilingualAttributeValueToNullRemovesAllItsValues", true);
        final OWLClassY y = persistYWithPluralMultilingualAttributeValues();

        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.getPluralString().clear();
        });

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertThat(result.getPluralString(), anyOf(nullValue(), empty()));
    }

    @Test
    void persistSupportsOntoDriverLangStringAttribute() throws Exception {
        this.em = getEntityManager("persistSupportsOntoDriverLangStringAttribute", true);
        entityM.setLangString(new LangString("testovaci hodnota", "cs"));
        persist(entityM);

        verifyStatementsPresent(Collections.singleton(new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_langString), entityM.getLangString()
                .getValue(), entityM.getLangString().getLanguage()
                .get())), em);
    }

    @Test
    void entityLoadingSupportsOntoDriverLangStringAttribute() throws Exception {
        this.em = getEntityManager("entityLoadingSupportsOntoDriverLangStringAttribute", true);
        final String langStringValue = "test value";
        final String language = "en";
        persistTestData(Arrays.asList(
                new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_langString), langStringValue, language)
        ), em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(new LangString(langStringValue, language), result.getLangString());
    }
}
