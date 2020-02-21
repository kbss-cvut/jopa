/**
 * Copyright (C) 2020 Czech Technical University in Prague
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