/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;

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
        set.add(OWLClassU.class);
        set.add(OWLClassWithQueryAttr.class);
        set.add(Person.class);
        set.add(Phone.class);
        return set;
    }

    private final PersistenceUnitClassFinder sut = new PersistenceUnitClassFinder();

    @BeforeEach
    void setUp() {
        TestClasspathScanner.invoked = false;
    }

    @Test
    public void throwsExceptionWhenScanPackageIsNotSupplied() {
        final Map<String, String> properties = Collections.emptyMap();
        assertThrows(IllegalArgumentException.class, () -> sut.scanClasspath(new Configuration(properties)));
    }

    @Test
    public void throwsExceptionWhenScanPackageIsEmpty() {
        final Map<String, String> properties = Collections.singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "");
        assertThrows(IllegalArgumentException.class, () -> sut.scanClasspath(new Configuration(properties)));
    }

    @Test
    public void doesNotFailWhenUnknownPackageNameIsPassed() {
        final Map<String, String> properties = Collections
                .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "com.cvut");
        sut.scanClasspath(new Configuration(properties));
        assertTrue(sut.getEntities().isEmpty());
    }

    @Test
    public void loadsEntityClassesWhenCorrectPackageIsSet() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        sut.scanClasspath(new Configuration(properties));
        assertEquals(ENTITY_CLASSES, sut.getEntities());
    }

    @Test
    public void loadsEntityClassesWhenAncestorPackageIsSet() {
        final Map<String, String> properties = new HashMap<>();
        properties.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");
        properties.put(JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS,
                       cz.cvut.kbss.jopa.environment.utils.TestClasspathScanner.class.getCanonicalName());
        sut.scanClasspath(new Configuration(properties));
        assertTrue(sut.getEntities().containsAll(ENTITY_CLASSES));
    }

    /**
     * Bug #5.
     */
    @Test
    public void entityLoadHandlesEntityNameContainingClassStringWhenProcessingJar() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test.jar");
        sut.scanClasspath(new Configuration(properties));
        final Set<Class<?>> result = sut.getEntities();
        final Optional<Class<?>> cls = result.stream().filter(c -> c.getName().contains("classInName"))
                                             .findAny();
        assertTrue(cls.isPresent());
    }

    @Test
    public void scanLoadsResultSetMappings() {
        final Map<String, String> properties = Collections.singletonMap(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        sut.scanClasspath(new Configuration(properties));
        assertFalse(sut.getResultSetMappings().isEmpty());
        assertTrue(sut.getResultSetMappings()
                      .contains(OWLClassA.class.getDeclaredAnnotation(SparqlResultSetMapping.class)));
    }

    @Test
    void usesConfiguredClasspathScanner() {
        final Map<String, String> properties = new HashMap<>();
        properties.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        properties.put(JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS, TestClasspathScanner.class.getName());
        sut.scanClasspath(new Configuration(properties));
        assertTrue(TestClasspathScanner.invoked);
    }

    public static class TestClasspathScanner implements ClasspathScanner {

        private static boolean invoked = false;

        public TestClasspathScanner() {
        }

        @Override
        public void addListener(Consumer<Class<?>> listener) {
            // Do nothing
        }

        @Override
        public void processClasses(String scanPackage) {
            invoked = true;
        }
    }
}
