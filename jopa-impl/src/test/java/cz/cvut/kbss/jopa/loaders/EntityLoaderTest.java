/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EntityLoaderTest {

    private static final Set<Class<?>> ENTITY_CLASSES = initEntityClasses();

    private static Set<Class<?>> initEntityClasses() {
        final Set<Class<?>> set = new HashSet<>();
        set.add(OWLClassA.class);
        set.add(OWLClassB.class);
        set.add(OWLClassC.class);
        set.add(OWLClassD.class);
        set.add(OWLClassE.class);
        set.add(OWLClassF.class);
        set.add(OWLClassG.class);
        set.add(OWLClassH.class);
        set.add(OWLClassI.class);
        set.add(OWLClassJ.class);
        set.add(OWLClassK.class);
        set.add(OWLClassL.class);
        set.add(OWLClassM.class);
        set.add(OWLClassN.class);
        set.add(OWLClassO.class);
        set.add(OWLClassP.class);
        set.add(OWLClassQ.class);
        set.add(OWLClassR.class);
        set.add(OWLClassS.class);
        return set;
    }

    private EntityLoader entityLoader = new EntityLoader();

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsNotSupplied() throws Exception {
        final Map<String, String> properties = Collections.emptyMap();
        entityLoader.discoverEntityClasses(new Configuration(properties));
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsEmpty() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "");
        entityLoader.discoverEntityClasses(new Configuration(properties));
    }

    @Test
    public void doesNotFailWhenUnknownPackageNameIsPassed() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "com.cvut");
        final Set<Class<?>> result = entityLoader.discoverEntityClasses(new Configuration(properties));
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadsEntityClassesWhenCorrectPackageIsSet() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        final Set<Class<?>> result = entityLoader.discoverEntityClasses(new Configuration(properties));
        assertEquals(ENTITY_CLASSES, result);
    }

    @Test
    public void loadsEntityClassesWhenAncestorPackageIsSet() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss");
        final Set<Class<?>> result = entityLoader.discoverEntityClasses(new Configuration(properties));
        assertTrue(result.containsAll(ENTITY_CLASSES));
    }

    /**
     * Bug #5.
     */
    @Test
    public void entityLoadHandlesEntityNameContainingClassStringWhenProcessingJar() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test.jar");
        final Set<Class<?>> result = entityLoader.discoverEntityClasses(new Configuration(properties));
        final Optional<Class<?>> cls = result.stream().filter(c -> c.getName().contains("classInName"))
                                             .findAny();
        assertTrue(cls.isPresent());
    }
}