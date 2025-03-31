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
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.NonPersistentClass;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class EntityLoaderTest {

    private final EntityLoader sut = new EntityLoader();

    @Test
    public void entityLoaderAddsEntityClassToEntities() {
        sut.accept(OWLClassA.class);
        assertTrue(sut.getEntities().contains(OWLClassA.class));
    }

    @Test
    public void entityLoaderIgnoresNonEntityClass() {
        sut.accept(String.class);
        assertFalse(sut.getEntities().contains(String.class));
    }

    @Test
    public void entityLoaderAcceptsInterfaceWithOwlClassAnnotation() {
        sut.accept(AnnotatedInterface.class);
        assertTrue(sut.getEntities().contains(AnnotatedInterface.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "interface")
    interface AnnotatedInterface {
    }
    interface UnAnnotatedInterface{

    }

    @Test
    public void entityLoaderIgnoresInterfaceWithoutOwlClassAnnotation() {
        sut.accept(UnAnnotatedInterface.class);
        assertFalse(sut.getEntities().contains(UnAnnotatedInterface.class));
    }

    @Test
    void entityLoaderIgnoresClassAnnotatedWithNonEntity() {
        sut.accept(NonPersistentClass.class);
        assertFalse(sut.getEntities().contains(NonPersistentClass.class));
    }
}
