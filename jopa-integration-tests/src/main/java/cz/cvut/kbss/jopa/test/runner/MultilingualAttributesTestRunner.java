package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.test.OWLClassY;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

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
        entityY.getSingularString().set("building", "en");
        entityY.getSingularString().set("stavba", "cs");
        entityY.getSingularString().set("der Bau", "de");
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
        y.getSingularString().set("building", "en");
        persist(y);

        y.getSingularString().set("stavba", "cs");
        y.getSingularString().set("der Bau", "de");
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
        y.getSingularString().set("construction", "en");
        persist(y);

        final String replacement = "building";
        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.getSingularString().set(replacement, "en");
            toUpdate.getSingularString().set("stavba", "cs");
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
        y.getSingularString().set("construction", "en");
        persist(y);

        final String replacement = "building";
        transactional(() -> {
            final OWLClassY toUpdate = findRequired(OWLClassY.class, y.getUri());
            toUpdate.setSingularString(new MultilingualString());
            toUpdate.getSingularString().set(replacement, "en");
            toUpdate.getSingularString().set("stavba", "cs");
        });

        final OWLClassY result = findRequired(OWLClassY.class, y.getUri());
        assertEquals(2, result.getSingularString().getLanguages().size());
        assertEquals(replacement, result.getSingularString().get("en"));
        assertEquals("stavba", result.getSingularString().get("cs"));
    }

    @Test
    void persistSupportsPluralMultilingualAttributes() {
        this.em = getEntityManager("persistSupportsPluralMultilingualAttributes", false);
        final OWLClassY y = new OWLClassY();
        final MultilingualString sOne = new MultilingualString();
        sOne.set("construction", "en");
        sOne.set("stavba", "cs");
        final MultilingualString sTwo = new MultilingualString();
        sTwo.set("building", "en");
        sTwo.set("budova", "cs");
        y.setPluralString(new HashSet<>(Arrays.asList(sOne, sTwo)));
        persist(y);

        final URI property = URI.create(Vocabulary.P_Y_PLURAL_MULTILINGUAL_ATTRIBUTE);
        transactionalThrowing(
                () -> verifyStatementsPresent(Arrays.asList(new Quad(y.getUri(), property, "construction", "en"),
                        new Quad(y.getUri(), property, "stavba", "cs"),
                        new Quad(y.getUri(), property, "building", "en"),
                        new Quad(y.getUri(), property, "budova", "cs")), em));
    }
}
