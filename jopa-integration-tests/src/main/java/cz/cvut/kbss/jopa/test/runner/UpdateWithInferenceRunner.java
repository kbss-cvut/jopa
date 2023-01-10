package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassW;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Testing implementation of Feature #121 - editable inferred attributes.
 */
public abstract class UpdateWithInferenceRunner extends BaseRunner {

    public UpdateWithInferenceRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void settingInferredAttributeFromNullToNewValueWorks() {
        final OWLClassF entityF = new OWLClassF(Generators.generateUri());
        this.em = getEntityManager("settingInferredAttributeFromNullToNewValueWorks", false);
        persist(entityF);

        final String updateValue = "updated value";
        em.getTransaction().begin();
        final OWLClassF f = findRequired(OWLClassF.class, entityF.getUri());
        assertDoesNotThrow(() -> f.setSecondStringAttribute(updateValue));
        em.getTransaction().commit();

        final OWLClassF result = findRequired(OWLClassF.class, entityF.getUri());
        assertEquals(updateValue, result.getSecondStringAttribute());
    }

    @Test
    public void additiveChangeToAttributeWithInferredValuesWorks() throws Exception {
        final OWLClassW entityW = new OWLClassW();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        final URI newType = Generators.generateUri();
        transactional(() -> {
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertFalse(toUpdate.getTypes().isEmpty());
            toUpdate.getTypes().add(newType);
        });

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), hasItem(newType));
    }

    @Test
    public void removalOfAssertedValueOfInferredAttributeWorks() throws Exception {
        final URI typeToRemove = Generators.generateUri();
        final OWLClassW entityW = new OWLClassW();
        entityW.setTypes(Collections.singleton(typeToRemove));
        final URI typeToAdd = Generators.generateUri();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         URI.create(Vocabulary.C_OWL_CLASS_A))), em);
        persist(entityW);

        transactional(() -> {
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertThat(toUpdate.getTypes(), hasItem(typeToRemove));
            toUpdate.getTypes().remove(typeToRemove);
            toUpdate.getTypes().add(typeToAdd);
        });

        final OWLClassW result = findRequired(OWLClassW.class, entityW.getUri());
        assertThat(result.getTypes(), hasItem(typeToAdd));
        assertThat(result.getTypes(), not(hasItem(typeToRemove)));
    }

    @Test
    public void removalOfInferredValueOfInferredAttributeThrowsInferredAttributeModifiedException() throws Exception {
        final URI typeToRemove = URI.create(Vocabulary.C_OWL_CLASS_A);
        final OWLClassW entityW = new OWLClassW();
        persistTestData(Collections.singleton(
                new Quad(URI.create(Vocabulary.C_OWL_CLASS_W), URI.create(RDFS.SUB_CLASS_OF),
                         typeToRemove)), em);
        persist(entityW);

        assertThrows(InferredAttributeModifiedException.class, () -> {
            em.getTransaction().begin();
            final OWLClassW toUpdate = findRequired(OWLClassW.class, entityW.getUri());
            assertThat(toUpdate.getTypes(), hasItem(typeToRemove));
            toUpdate.getTypes().remove(typeToRemove);
            em.getTransaction().commit();
        });
    }
}
