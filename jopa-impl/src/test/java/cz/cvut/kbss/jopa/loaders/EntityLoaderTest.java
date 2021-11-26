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
package cz.cvut.kbss.jopa.loaders;

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
    public void entityLoaderIgnoresInterfaceWithOwlClassAnnotation() {
        sut.accept(AnnotatedInterface.class);
        assertFalse(sut.getEntities().contains(AnnotatedInterface.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "interface")
    interface AnnotatedInterface {
    }
}