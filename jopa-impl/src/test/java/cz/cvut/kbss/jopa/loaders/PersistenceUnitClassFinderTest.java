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
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.*;

public class PersistenceUnitClassFinderTest {

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
        set.add(OWLClassT.class);
        return set;
    }

    private PersistenceUnitClassFinder finder = new PersistenceUnitClassFinder();

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsNotSupplied() {
        final Map<String, String> properties = Collections.emptyMap();
        finder.scanClasspath(new Configuration(properties));
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsEmpty() {
        final Map<String, String> properties = Collections.singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "");
        finder.scanClasspath(new Configuration(properties));
    }

    @Test
    public void doesNotFailWhenUnknownPackageNameIsPassed() {
        final Map<String, String> properties = Collections
                .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "com.cvut");
        finder.scanClasspath(new Configuration(properties));
        assertTrue(finder.getEntities().isEmpty());
    }

    @Test
    public void loadsEntityClassesWhenCorrectPackageIsSet() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        finder.scanClasspath(new Configuration(properties));
        assertEquals(ENTITY_CLASSES, finder.getEntities());
    }

    @Test
    public void loadsEntityClassesWhenAncestorPackageIsSet() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss");
        finder.scanClasspath(new Configuration(properties));
        assertTrue(finder.getEntities().containsAll(ENTITY_CLASSES));
    }

    /**
     * Bug #5.
     */
    @Test
    public void entityLoadHandlesEntityNameContainingClassStringWhenProcessingJar() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test.jar");
        finder.scanClasspath(new Configuration(properties));
        final Set<Class<?>> result = finder.getEntities();
        final Optional<Class<?>> cls = result.stream().filter(c -> c.getName().contains("classInName"))
                                             .findAny();
        assertTrue(cls.isPresent());
    }

    @Test
    public void scanLoadsResultSetMappings() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        finder.scanClasspath(new Configuration(properties));
        assertFalse(finder.getResultSetMappings().isEmpty());
        assertTrue(finder.getResultSetMappings()
                         .contains(OWLClassA.class.getDeclaredAnnotation(SparqlResultSetMapping.class)));
    }
}