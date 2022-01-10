/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public class RepositoryMapTest {

    private static final URI CONTEXT = URI.create("http://repositoryMapTestContext");

    private static OWLClassA entityA;
    private static OWLClassA cloneA;
    private static OWLClassB entityB;
    private static OWLClassB cloneB;
    private static OWLClassE entityE;
    private static OWLClassE cloneE;
    private static Descriptor descriptor;

    private RepositoryMap repoMap;

    private Map<Set<URI>, Map<Object, Object>> theMap;
    private Map<Object, Descriptor> descriptors;

    @BeforeAll
    public static void setUpBeforeClass() {
        entityA = new OWLClassA();
        entityA.setStringAttribute("SomeString");
        cloneA = new OWLClassA();
        cloneA.setStringAttribute(entityA.getStringAttribute());
        entityB = new OWLClassB();
        entityB.setStringAttribute("otherString");
        cloneB = new OWLClassB();
        cloneB.setStringAttribute(entityB.getStringAttribute());
        entityE = new OWLClassE();
        cloneE = new OWLClassE();
        descriptor = new EntityDescriptor(CONTEXT);
    }

    @SuppressWarnings("unchecked")
    @BeforeEach
    public void setUp() throws Exception {
        initData();
        this.repoMap = new RepositoryMap();
        final Field mapField = RepositoryMap.class.getDeclaredField("origsToClones");
        mapField.setAccessible(true);
        this.theMap = (Map<Set<URI>, Map<Object, Object>>) mapField.get(repoMap);
        final Field descriptorsField = RepositoryMap.class.getDeclaredField("entityDescriptors");
        descriptorsField.setAccessible(true);
        this.descriptors = (Map<Object, Descriptor>) descriptorsField.get(repoMap);
    }

    private void initData() {
        entityA.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityA"));
        cloneA.setUri(URI.create(entityA.getUri().toString()));
        entityB.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityB"));
        cloneB.setUri(URI.create(entityB.getUri().toString()));
        entityE.setUri(URI.create("http://krizik.felk.cvut.cz/jopa/ontology/entityE"));
        cloneE.setUri(URI.create(entityE.getUri().toString()));
    }

    @Test
    public void testAdd() {
        repoMap.add(descriptor, entityA, cloneA);
        assertTrue(repoMap.contains(descriptor, entityA));
        assertTrue(theMap.containsKey(Collections.singleton(CONTEXT)));
        assertTrue(theMap.get(Collections.singleton(CONTEXT)).containsKey(entityA));
    }

    @Test
    public void testRemove() {
        repoMap.add(descriptor, entityA, cloneA);
        repoMap.add(descriptor, entityB, cloneB);
        assertTrue(repoMap.contains(descriptor, entityA));
        assertTrue(repoMap.contains(descriptor, entityB));

        repoMap.remove(descriptor, entityB);
        assertTrue(repoMap.contains(descriptor, entityA));
        assertFalse(repoMap.contains(descriptor, entityB));
    }

    @Test
    public void testGet() {
        repoMap.add(descriptor, entityA, cloneA);
        repoMap.add(descriptor, entityB, cloneB);

        final Object res = repoMap.get(descriptor, entityA);
        assertNotNull(res);
        assertSame(cloneA, res);
    }

    @Test
    public void testGetUnknown() {
        final Object res = repoMap.get(descriptor, entityA);
        assertNull(res);
    }

    @Test
    public void testContains() {
        repoMap.add(descriptor, entityA, cloneA);
        repoMap.add(descriptor, entityB, cloneB);

        assertTrue(repoMap.contains(descriptor, entityA));
        assertTrue(repoMap.contains(descriptor, entityB));
        assertFalse(repoMap.contains(descriptor, entityE));
        final EntityDescriptor desc = new EntityDescriptor();
        assertFalse(repoMap.contains(desc, entityA));
    }

    @Test
    public void testAddEntityToRepository() {
        repoMap.addEntityToRepository(entityA, descriptor);
        assertTrue(descriptors.containsKey(entityA));
        assertSame(descriptor, descriptors.get(entityA));
    }

    @Test
    public void testRemoveEntityToRepository() {
        repoMap.addEntityToRepository(entityA, descriptor);
        assertTrue(descriptors.containsKey(entityA));

        repoMap.removeEntityToRepository(entityA);
        assertFalse(descriptors.containsKey(entityA));
    }

    @Test
    public void testGetEntityDescriptor() {
        repoMap.addEntityToRepository(entityA, descriptor);
        assertTrue(descriptors.containsKey(entityA));

        final Descriptor res = repoMap.getEntityDescriptor(entityA);
        assertNotNull(res);
        assertSame(descriptor, res);
    }

    @Test
    public void testClear() {
        repoMap.add(descriptor, entityA, cloneA);
        repoMap.add(descriptor, entityB, cloneB);
        repoMap.addEntityToRepository(entityA, descriptor);

        repoMap.clear();
        assertFalse(repoMap.contains(descriptor, entityA));
        assertFalse(repoMap.contains(descriptor, entityB));
        assertNull(repoMap.getEntityDescriptor(entityA));
    }

    @Test
    public void addingTwoObjectsWithNullIdentifierIsPossible() {
        entityA.setUri(null);
        cloneA.setUri(null);
        entityB.setUri(null);
        cloneB.setUri(null);
        repoMap.add(descriptor, entityA, cloneA);
        assertTrue(theMap.get(Collections.singleton(CONTEXT)).containsValue(cloneA));
        repoMap.add(descriptor, entityB, cloneB);
        assertTrue(theMap.get(Collections.singleton(CONTEXT)).containsValue(cloneA));
        assertTrue(theMap.get(Collections.singleton(CONTEXT)).containsValue(cloneB));
    }
}
