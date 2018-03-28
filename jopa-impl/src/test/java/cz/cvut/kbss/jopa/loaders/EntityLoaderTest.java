package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.junit.Test;

import static org.junit.Assert.*;

public class EntityLoaderTest {

    private final EntityLoader entityLoader = new EntityLoader();

    @Test
    public void entityLoaderAddsEntityClassToEntities() {
        entityLoader.accept(OWLClassA.class);
        assertTrue(entityLoader.getEntities().contains(OWLClassA.class));
    }

    @Test
    public void entityLoaderIgnoresNonEntityClass() {
        entityLoader.accept(String.class);
        assertFalse(entityLoader.getEntities().contains(String.class));
    }

    @Test
    public void entityLoaderIgnoresInterfaceWithOwlClassAnnotation() {
        entityLoader.accept(AnnotatedInterface.class);
        assertFalse(entityLoader.getEntities().contains(AnnotatedInterface.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "interface")
    interface AnnotatedInterface {
    }
}