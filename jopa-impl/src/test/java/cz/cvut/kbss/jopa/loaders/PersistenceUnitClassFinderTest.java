/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassG;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassK;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.Phone;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class PersistenceUnitClassFinderTest {

    static final Set<Class<?>> ENTITY_CLASSES = initEntityClasses();

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

    @Test
    public void scanClasspathScansWholeClasspathWhenPackageIsNotProvided() {
        sut.scanClasspath(new Configuration(Map.of(JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS, TestClasspathScanner.class.getName())));
        assertEquals(Map.of(0, ""), TestClasspathScanner.invocations);
    }

    @Test
    public void scanClasspathScansWholeClasspathWhenPackageIsEmpty() {
        sut.scanClasspath(new Configuration(Map.of(
                JOPAPersistenceProperties.SCAN_PACKAGE, "",
                JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS, TestClasspathScanner.class.getName()
        )));
        assertEquals(Map.of(0, ""), TestClasspathScanner.invocations);
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
        assertEquals(Map.of(0, "cz.cvut.kbss.jopa.environment"), TestClasspathScanner.invocations);
    }

    @Test
    void scanClasspathUsesClasspathScannerToFindEntityClassesInAllProvidedPackages() {
        sut.scanClasspath(new Configuration(Map.of(
                JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment,org.example",
                JOPAPersistenceProperties.CLASSPATH_SCANNER_CLASS, TestClasspathScanner.class.getName()
        )));
        assertEquals(Map.of(0, "cz.cvut.kbss.jopa.environment",
                1, "org.example"), TestClasspathScanner.invocations);
    }

    public static class TestClasspathScanner implements ClasspathScanner {

        private static Map<Integer, String> invocations;

        public TestClasspathScanner() {
            invocations = new LinkedHashMap<>();
        }

        @Override
        public void addListener(Consumer<Class<?>> listener) {
            // Do nothing
        }

        @Override
        public void processClasses(String scanPackage) {
            invocations.put(invocations.size(), scanPackage);
        }
    }
}
