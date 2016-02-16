package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.owlapi.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
}